open ChainWorkerTypes
open Belt
type t = {
  chainConfig: Config.chainConfig,
  serverUrl: string,
}

module Helpers = {
  let rec queryLogsPageWithBackoff = async (
    ~backoffMsOnFailure=200,
    ~callDepth=0,
    ~maxCallDepth=15,
    query: unit => promise<HyperSync.queryResponse<HyperSync.logsQueryPage>>,
    logger: Pino.t,
  ) =>
    switch await query() {
    | Error(e) =>
      let msg = e->HyperSync.queryErrorToMsq
      if callDepth < maxCallDepth {
        logger->Logging.childWarn({
          "err": msg,
          "msg": `Issue while running fetching of events from Hypersync endpoint. Will wait ${backoffMsOnFailure->Belt.Int.toString}ms and try again.`,
          "type": "EXPONENTIAL_BACKOFF",
        })
        await Time.resolvePromiseAfterDelay(~delayMilliseconds=backoffMsOnFailure)
        await queryLogsPageWithBackoff(
          ~callDepth=callDepth + 1,
          ~backoffMsOnFailure=2 * backoffMsOnFailure,
          query,
          logger,
        )
      } else {
        logger->Logging.childError({
          "err": msg,
          "msg": `Issue while running fetching batch of events from Hypersync endpoint. Attempted query a maximum of ${maxCallDepth->string_of_int} times. Will NOT retry.`,
          "type": "EXPONENTIAL_BACKOFF_MAX_DEPTH",
        })
        Js.Exn.raiseError(msg)
      }
    | Ok(v) => v
    }

  type jsExnObj = {
    name?: string,
    message?: string,
    stack?: string,
  }

  let makeJsExnParams = exn => {
    open Js.Exn
    let name = exn->name
    let message = exn->message
    let stack = exn->stack
    {?name, ?message, ?stack}
  }

  exception JsExn(jsExnObj)
  exception ErrorMessage(string)
}

let make = (chainConfig: Config.chainConfig, ~serverUrl): t => {
  {
    chainConfig,
    serverUrl,
  }
}

/**
Holds the value of the next page fetch happening concurrently to current page processing
*/
type nextPageFetchRes = {
  contractInterfaceManager: ContractInterfaceManager.t,
  page: HyperSync.logsQueryPage,
  pageFetchTime: int,
}

let waitForBlockGreaterThanCurrentHeight = ({serverUrl}: t, ~currentBlockHeight, ~logger) => {
  HyperSync.pollForHeightGtOrEq(~serverUrl, ~blockNumber=currentBlockHeight, ~logger)
}

let waitForNextBlockBeforeQuery = async (
  ~serverUrl,
  ~fromBlock,
  ~currentBlockHeight,
  ~logger,
  ~setCurrentBlockHeight,
) => {
  if fromBlock > currentBlockHeight {
    logger->Logging.childTrace("Worker is caught up, awaiting new blocks")

    //If the block we want to query from is greater than the current height,
    //poll for until the archive height is greater than the from block and set
    //current height to the new height
    let currentBlockHeight = await HyperSync.pollForHeightGtOrEq(
      ~serverUrl,
      ~blockNumber=fromBlock,
      ~logger,
    )

    setCurrentBlockHeight(currentBlockHeight)
  }
}

let getNextPage = async (
  {serverUrl, chainConfig}: t,
  ~fromBlock,
  ~toBlock,
  ~currentBlockHeight,
  ~logger,
  ~setCurrentBlockHeight,
  ~contractAddressMapping,
) => {
  //Wait for a valid range to query
  //This should never have to wait since we check that the from block is below the toBlock
  //this in the GlobalState reducer
  await waitForNextBlockBeforeQuery(
    ~serverUrl,
    ~fromBlock,
    ~currentBlockHeight,
    ~setCurrentBlockHeight,
    ~logger,
  )

  //Instantiate each time to add new registered contract addresses
  let contractInterfaceManager = ContractInterfaceManager.make(
    ~chainConfig,
    ~contractAddressMapping,
  )

  let contractAddressesAndtopics =
    contractInterfaceManager->ContractInterfaceManager.getAllContractTopicsAndAddresses

  let startFetchingBatchTimeRef = Hrtime.makeTimer()

  //fetch batch
  let pageUnsafe = await Helpers.queryLogsPageWithBackoff(
    () => HyperSync.queryLogsPage(~serverUrl, ~fromBlock, ~toBlock, ~contractAddressesAndtopics),
    logger,
  )

  let pageFetchTime =
    startFetchingBatchTimeRef->Hrtime.timeSince->Hrtime.toMillis->Hrtime.intFromMillis

  {page: pageUnsafe, contractInterfaceManager, pageFetchTime}
}

let fetchBlockRange = async (
  self: t,
  ~query: blockRangeFetchArgs,
  ~logger,
  ~currentBlockHeight,
  ~setCurrentBlockHeight,
) => {
  let mkLogAndRaise = ErrorHandling.mkLogAndRaise(~logger)
  try {
    let {chainConfig: {chain}, serverUrl} = self
    let {fetchStateRegisterId, fromBlock, contractAddressMapping, toBlock, ?eventFilters} = query
    let startFetchingBatchTimeRef = Hrtime.makeTimer()
    //fetch batch
    let {page: pageUnsafe, contractInterfaceManager, pageFetchTime} =
      await self->getNextPage(
        ~fromBlock,
        ~toBlock,
        ~currentBlockHeight,
        ~contractAddressMapping,
        ~logger,
        ~setCurrentBlockHeight,
      )

    //set height and next from block
    let currentBlockHeight = pageUnsafe.archiveHeight

    logger->Logging.childTrace({
      "message": "Retrieved event page from server",
      "fromBlock": fromBlock,
      "toBlock": pageUnsafe.nextBlock - 1,
    })

    //The heighest (biggest) blocknumber that was accounted for in
    //Our query. Not necessarily the blocknumber of the last log returned
    //In the query
    let heighestBlockQueried = pageUnsafe.nextBlock - 1

    let lastBlockQueriedPromise: promise<
      ReorgDetection.blockData,
    > = switch pageUnsafe.rollbackGuard {
    //In the case a rollbackGuard is returned (this only happens at the head for unconfirmed blocks)
    //use these values
    | Some({blockNumber, timestamp, hash}) =>
      {
        ReorgDetection.blockNumber,
        blockTimestamp: timestamp,
        blockHash: hash,
      }->Promise.resolve
    | None =>
      //The optional block and timestamp of the last item returned by the query
      //(Optional in the case that there are no logs returned in the query)
      switch pageUnsafe.items->Belt.Array.get(pageUnsafe.items->Belt.Array.length - 1) {
      | Some({log: {blockNumber, blockHash}, blockTimestamp})
        //If the last log item in the current page is equal to the
        //heighest block acounted for in the query. Simply return this
        //value without making an extra query
        if blockNumber == heighestBlockQueried =>
        {
          ReorgDetection.blockNumber,
          blockTimestamp,
          blockHash,
        }->Promise.resolve
      //If it does not match it means that there were no matching logs in the last
      //block so we should fetch the block data
      | Some(_)
      | None =>
        //If there were no logs at all in the current page query then fetch the
        //timestamp of the heighest block accounted for
        HyperSync.queryBlockData(
          ~serverUrl,
          ~blockNumber=heighestBlockQueried,
        )->Promise.thenResolve(res =>
          switch res {
          | Ok(Some(blockData)) => blockData
          | Ok(None) =>
            mkLogAndRaise(
              Not_found,
              ~msg=`Failure, blockData for block ${heighestBlockQueried->Int.toString} unexpectedly returned None`,
            )
          | Error(e) =>
            Helpers.ErrorMessage(HyperSync.queryErrorToMsq(e))->mkLogAndRaise(
              ~msg=`Failed to query blockData for block ${heighestBlockQueried->Int.toString}`,
            )
          }
        )
      }
    }

    let parsingTimeRef = Hrtime.makeTimer()

    //Parse page items into queue items
    let parsedQueueItemsPreFilter = if Config.shouldUseHypersyncClientDecoder {
      //Currently there are still issues with decoder for some cases so
      //this can only be activated with a flag
      let decoder = switch contractInterfaceManager
      ->ContractInterfaceManager.getAbiMapping
      ->HyperSyncClient.Decoder.make {
      | exception exn =>
        exn->mkLogAndRaise(
          ~msg="Failed to instantiate a decoder from hypersync client, please double check your ABI.",
        )
      | decoder => decoder
      }
      //Parse page items into queue items
      let parsedEvents = switch await decoder->HyperSyncClient.Decoder.decodeEvents(
        pageUnsafe.events,
      ) {
      | exception exn =>
        exn->mkLogAndRaise(
          ~msg="Failed to parse events using hypersync client, please double check your ABI.",
        )
      | parsedEvents => parsedEvents
      }

      pageUnsafe.items
      ->Belt.Array.zip(parsedEvents)
      ->Belt.Array.map(((item, event)): Types.eventBatchQueueItem => {
        let {blockTimestamp, log: {blockNumber, logIndex}} = item
        let chainId = chain->ChainMap.Chain.toChainId
        {
          timestamp: blockTimestamp,
          chain,
          blockNumber,
          logIndex,
          event: switch event
          ->Belt.Option.getExn
          ->Converters.convertDecodedEvent(
            ~contractInterfaceManager,
            ~log=item.log,
            ~blockTimestamp,
            ~chainId,
            ~txOrigin=item.txOrigin,
            ~txTo=item.txTo,
          ) {
          | Ok(v) => v
          | Error(exn) =>
            let logger = Logging.createChildFrom(
              ~logger,
              ~params={"chainId": chainId, "blockNumber": blockNumber, "logIndex": logIndex},
            )
            exn->ErrorHandling.mkLogAndRaise(~msg="Failed to convert decoded event", ~logger)
          },
        }
      })
    } else {
      //Parse with viem -> slower than the HyperSyncClient
      pageUnsafe.items->Array.map(item => {
        let {log: {blockNumber, logIndex}} = item
        let chainId = chain->ChainMap.Chain.toChainId
        switch Converters.parseEvent(
          ~log=item.log,
          ~blockTimestamp=item.blockTimestamp,
          ~contractInterfaceManager,
          ~chainId,
          ~txOrigin=item.txOrigin,
          ~txTo=item.txTo,
        ) {
        | Ok(parsed) =>
          (
            {
              timestamp: item.blockTimestamp,
              chain,
              blockNumber,
              logIndex,
              event: parsed,
            }: Types.eventBatchQueueItem
          )

        | Error(exn) =>
          let params = {
            "chainId": chainId,
            "blockNumber": blockNumber,
            "logIndex": logIndex,
          }
          let logger = Logging.createChildFrom(~logger, ~params)
          exn->ErrorHandling.mkLogAndRaise(
            ~msg="Failed to parse event with viem, please double check your ABI.",
            ~logger,
          )
        }
      })
    }

    let parsedQueueItems = switch eventFilters {
    //Most cases there are no filters so this will be passed throug
    | None => parsedQueueItemsPreFilter
    | Some(eventFilters) =>
      //In the case where there are filters, apply them and keep the events that
      //are needed
      parsedQueueItemsPreFilter->Array.keep(FetchState.applyFilters(~eventFilters))
    }

    let parsingTimeElapsed = parsingTimeRef->Hrtime.timeSince->Hrtime.toMillis->Hrtime.intFromMillis

    let lastBlockScannedData = await lastBlockQueriedPromise

    let reorgGuard = {
      lastBlockScannedData,
      firstBlockParentNumberAndHash: pageUnsafe.rollbackGuard->Option.map(v => {
        ReorgDetection.blockHash: v.firstParentHash,
        blockNumber: v.firstBlockNumber - 1,
      }),
    }

    let totalTimeElapsed =
      startFetchingBatchTimeRef->Hrtime.timeSince->Hrtime.toMillis->Hrtime.intFromMillis

    let stats = {
      totalTimeElapsed,
      parsingTimeElapsed,
      pageFetchTime,
      averageParseTimePerLog: parsingTimeElapsed->Belt.Int.toFloat /.
        parsedQueueItems->Array.length->Belt.Int.toFloat,
    }

    {
      latestFetchedBlockTimestamp: lastBlockScannedData.blockTimestamp,
      parsedQueueItems,
      heighestQueriedBlockNumber: lastBlockScannedData.blockNumber,
      stats,
      currentBlockHeight,
      reorgGuard,
      fromBlockQueried: fromBlock,
      fetchStateRegisterId,
      worker: HyperSync(self),
    }->Ok
  } catch {
  | exn => exn->ErrorHandling.make(~logger, ~msg="Failed to fetch block Range")->Error
  }
}

let getBlockHashes = ({serverUrl}: t, ~blockNumbers) =>
  HyperSync.queryBlockDataMulti(~serverUrl, ~blockNumbers)->Promise.thenResolve(HyperSync.mapExn)
