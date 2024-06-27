open Belt

/***** TAKE NOTE ******
This is a hack to get genType to work!

In order for genType to produce recursive types, it needs to be at the 
root module of a file. If it's defined in a nested module it does not 
work. So all the MockDb types and internal functions are defined in TestHelpers_MockDb
and only public functions are recreated and exported from this module.

the following module:
```rescript
module MyModule = {
  @genType
  type rec a = {fieldB: b}
  @genType and b = {fieldA: a}
}
```

produces the following in ts:
```ts
// tslint:disable-next-line:interface-over-type-literal
export type MyModule_a = { readonly fieldB: b };

// tslint:disable-next-line:interface-over-type-literal
export type MyModule_b = { readonly fieldA: MyModule_a };
```

fieldB references type b which doesn't exist because it's defined
as MyModule_b
*/

module MockDb = {
  @genType
  let createMockDb = TestHelpers_MockDb.createMockDb
}

@genType
module Addresses = {
  include TestHelpers_MockAddresses
}

module EventFunctions = {
  //Note these are made into a record to make operate in the same way
  //for Res, JS and TS.

  /**
  The arguements that get passed to a "processEvent" helper function
  */
  @genType
  type eventProcessorArgs<'eventArgs> = {
    event: Types.eventLog<'eventArgs>,
    mockDb: TestHelpers_MockDb.t,
    chainId?: int,
  }

  /**
  The default chain ID to use (ethereum mainnet) if a user does not specify int the 
  eventProcessor helper
  */
  let \"DEFAULT_CHAIN_ID" = try {
    ChainMap.Chain.all->Array.getExn(0)->ChainMap.Chain.toChainId
  } catch {
  | _ =>
    Js.Exn.raiseError("No default chain Id found, please add at least 1 chain to your config.yaml.")
  }

  /**
  A function composer to help create individual processEvent functions
  */
  let makeEventProcessor = (
    ~contextCreator: Context.contextCreator<
      'eventArgs,
      'loaderContext,
      'handlerContextSync,
      'handlerContextAsync,
    >,
    ~getLoader: unit => Handlers.loader<_>,
    ~eventWithContextAccessor: (
      Types.eventLog<'eventArgs>,
      Context.genericContextCreatorFunctions<
        'loaderContext,
        'handlerContextSync,
        'handlerContextAsync,
      >,
    ) => Context.eventAndContext,
    ~eventName: Types.eventName,
    ~cb: TestHelpers_MockDb.t => unit,
  ) => {
    ({event, mockDb, ?chainId}) => {
      RegisterHandlers.registerAllHandlers()
      //The user can specify a chainId of an event or leave it off
      //and it will default to "DEFAULT_CHAIN_ID"
      let chainId = chainId->Option.getWithDefault(\"DEFAULT_CHAIN_ID")

      //Create an individual logging context for traceability
      let logger = Logging.createChild(
        ~params={
          "Context": `Test Processor for ${eventName
            ->S.serializeToJsonStringWith(. Types.eventNameSchema)
            ->Result.getExn} Event`,
          "Chain ID": chainId,
          "event": event,
        },
      )

      //Deep copy the data in mockDb, mutate the clone and return the clone
      //So no side effects occur here and state can be compared between process
      //steps
      let mockDbClone = mockDb->TestHelpers_MockDb.cloneMockDb

      let asyncGetters: Context.entityGetters = {
        getEntryPoint_AccountDeployed: async id =>
          mockDbClone.entities.entryPoint_AccountDeployed.get(id)->Belt.Option.mapWithDefault(
            [],
            entity => [entity],
          ),
        getEntryPoint_BeforeExecution: async id =>
          mockDbClone.entities.entryPoint_BeforeExecution.get(id)->Belt.Option.mapWithDefault(
            [],
            entity => [entity],
          ),
        getEntryPoint_Deposited: async id =>
          mockDbClone.entities.entryPoint_Deposited.get(id)->Belt.Option.mapWithDefault(
            [],
            entity => [entity],
          ),
        getEntryPoint_SignatureAggregatorChanged: async id =>
          mockDbClone.entities.entryPoint_SignatureAggregatorChanged.get(
            id,
          )->Belt.Option.mapWithDefault([], entity => [entity]),
        getEntryPoint_StakeLocked: async id =>
          mockDbClone.entities.entryPoint_StakeLocked.get(id)->Belt.Option.mapWithDefault(
            [],
            entity => [entity],
          ),
        getEntryPoint_StakeUnlocked: async id =>
          mockDbClone.entities.entryPoint_StakeUnlocked.get(id)->Belt.Option.mapWithDefault(
            [],
            entity => [entity],
          ),
        getEntryPoint_StakeWithdrawn: async id =>
          mockDbClone.entities.entryPoint_StakeWithdrawn.get(id)->Belt.Option.mapWithDefault(
            [],
            entity => [entity],
          ),
        getEntryPoint_UserOperationEvent: async id =>
          mockDbClone.entities.entryPoint_UserOperationEvent.get(id)->Belt.Option.mapWithDefault(
            [],
            entity => [entity],
          ),
        getEntryPoint_UserOperationRevertReason: async id =>
          mockDbClone.entities.entryPoint_UserOperationRevertReason.get(
            id,
          )->Belt.Option.mapWithDefault([], entity => [entity]),
        getEntryPoint_Withdrawn: async id =>
          mockDbClone.entities.entryPoint_Withdrawn.get(id)->Belt.Option.mapWithDefault(
            [],
            entity => [entity],
          ),
      }

      //Construct a new instance of an in memory store to run for the given event
      let inMemoryStore = IO.InMemoryStore.make()

      //Construct a context with the inMemory store for the given event to run
      //loaders and handlers
      let context = contextCreator(~event, ~inMemoryStore, ~chainId, ~logger, ~asyncGetters)

      let loaderContext = context.getLoaderContext()

      let loader = getLoader()

      //Run the loader, to get all the read values/contract registrations
      //into the context
      loader({event, context: loaderContext})

      //Get all the entities are requested to be loaded from the mockDB
      let entityBatch = context.getEntitiesToLoad()

      //Load requested entities from the cloned mockDb into the inMemoryStore
      mockDbClone->TestHelpers_MockDb.loadEntitiesToInMemStore(~entityBatch, ~inMemoryStore)

      //Run the event and handler context through the eventRouter
      //With inMemoryStore
      let eventAndContext: Context.eventRouterEventAndContext = {
        chainId,
        event: eventWithContextAccessor(event, context),
      }

      eventAndContext->EventProcessing.eventRouter(
        ~latestProcessedBlocks=EventProcessing.EventsProcessed.makeEmpty(),
        ~inMemoryStore,
        ~cb=res =>
          switch res {
          | Ok(_latestProcessedBlocks) =>
            //Now that the processing is finished. Simulate writing a batch
            //(Although in this case a batch of 1 event only) to the cloned mockDb
            mockDbClone->TestHelpers_MockDb.writeFromMemoryStore(~inMemoryStore)

            //Return the cloned mock db
            cb(mockDbClone)

          | Error(errHandler) =>
            errHandler->ErrorHandling.log
            errHandler->ErrorHandling.raiseExn
          },
      )
    }
  }

  /**Creates a mock event processor, wrapping the callback in a Promise for async use*/
  let makeAsyncEventProcessor = (
    ~contextCreator,
    ~getLoader,
    ~eventWithContextAccessor,
    ~eventName,
    eventProcessorArgs,
  ) => {
    Promise.make((res, _rej) => {
      makeEventProcessor(
        ~contextCreator,
        ~getLoader,
        ~eventWithContextAccessor,
        ~eventName,
        ~cb=mockDb => res(. mockDb),
        eventProcessorArgs,
      )
    })
  }

  /**
  Creates a mock event processor, exposing the return of the callback in the return,
  raises an exception if the handler is async
  */
  let makeSyncEventProcessor = (
    ~contextCreator,
    ~getLoader,
    ~eventWithContextAccessor,
    ~eventName,
    eventProcessorArgs,
  ) => {
    //Dangerously set to None, nextMockDb will be set in the callback
    let nextMockDb = ref(None)
    makeEventProcessor(
      ~contextCreator,
      ~getLoader,
      ~eventWithContextAccessor,
      ~eventName,
      ~cb=mockDb => nextMockDb := Some(mockDb),
      eventProcessorArgs,
    )

    //The callback is called synchronously so nextMockDb should be set.
    //In the case it's not set it would mean that the user is using an async handler
    //in which case we want to error and alert the user.
    switch nextMockDb.contents {
    | Some(mockDb) => mockDb
    | None =>
      Js.Exn.raiseError(
        "processEvent failed because handler is not synchronous, please use processEventAsync instead",
      )
    }
  }

  /**
  Optional params for all additional data related to an eventLog
  */
  @genType
  type mockEventData = {
    blockNumber?: int,
    blockTimestamp?: int,
    blockHash?: string,
    chainId?: int,
    srcAddress?: Ethers.ethAddress,
    transactionHash?: string,
    transactionIndex?: int,
    txOrigin?: option<Ethers.ethAddress>,
    txTo?: option<Ethers.ethAddress>,
    logIndex?: int,
  }

  /**
  Applies optional paramters with defaults for all common eventLog field
  */
  let makeEventMocker = (
    ~params: 'eventParams,
    ~mockEventData: option<mockEventData>,
  ): Types.eventLog<'eventParams> => {
    let {
      ?blockNumber,
      ?blockTimestamp,
      ?blockHash,
      ?srcAddress,
      ?chainId,
      ?transactionHash,
      ?transactionIndex,
      ?logIndex,
      ?txOrigin,
      ?txTo,
    } =
      mockEventData->Belt.Option.getWithDefault({})

    {
      params,
      txOrigin: txOrigin->Belt.Option.flatMap(i => i),
      txTo: txTo->Belt.Option.flatMap(i => i),
      chainId: chainId->Belt.Option.getWithDefault(1),
      blockNumber: blockNumber->Belt.Option.getWithDefault(0),
      blockTimestamp: blockTimestamp->Belt.Option.getWithDefault(0),
      blockHash: blockHash->Belt.Option.getWithDefault(Ethers.Constants.zeroHash),
      srcAddress: srcAddress->Belt.Option.getWithDefault(Addresses.defaultAddress),
      transactionHash: transactionHash->Belt.Option.getWithDefault(Ethers.Constants.zeroHash),
      transactionIndex: transactionIndex->Belt.Option.getWithDefault(0),
      logIndex: logIndex->Belt.Option.getWithDefault(0),
    }
  }
}

module EntryPoint = {
  module AccountDeployed = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.AccountDeployedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.AccountDeployed.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_AccountDeployedWithContext(event, context),
      ~eventName=Types.EntryPoint_AccountDeployed,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.AccountDeployedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.AccountDeployed.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_AccountDeployedWithContext(event, context),
      ~eventName=Types.EntryPoint_AccountDeployed,
    )

    @genType
    type createMockArgs = {
      userOpHash?: string,
      sender?: Ethers.ethAddress,
      factory?: Ethers.ethAddress,
      paymaster?: Ethers.ethAddress,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {?userOpHash, ?sender, ?factory, ?paymaster, ?mockEventData} = args

      let params: Types.EntryPointContract.AccountDeployedEvent.eventArgs = {
        userOpHash: userOpHash->Belt.Option.getWithDefault("foo"),
        sender: sender->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        factory: factory->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        paymaster: paymaster->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module BeforeExecution = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.BeforeExecutionEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.BeforeExecution.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_BeforeExecutionWithContext(event, context),
      ~eventName=Types.EntryPoint_BeforeExecution,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.BeforeExecutionEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.BeforeExecution.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_BeforeExecutionWithContext(event, context),
      ~eventName=Types.EntryPoint_BeforeExecution,
    )

    @genType
    type createMockArgs = {mockEventData?: EventFunctions.mockEventData}

    @genType
    let createMockEvent = args => {
      let {?mockEventData} = args

      let params: Types.EntryPointContract.BeforeExecutionEvent.eventArgs = ()

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module Deposited = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.DepositedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.Deposited.getLoader,
      ~eventWithContextAccessor=(event, context) => Context.EntryPointContract_DepositedWithContext(
        event,
        context,
      ),
      ~eventName=Types.EntryPoint_Deposited,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.DepositedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.Deposited.getLoader,
      ~eventWithContextAccessor=(event, context) => Context.EntryPointContract_DepositedWithContext(
        event,
        context,
      ),
      ~eventName=Types.EntryPoint_Deposited,
    )

    @genType
    type createMockArgs = {
      account?: Ethers.ethAddress,
      totalDeposit?: Ethers.BigInt.t,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {?account, ?totalDeposit, ?mockEventData} = args

      let params: Types.EntryPointContract.DepositedEvent.eventArgs = {
        account: account->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        totalDeposit: totalDeposit->Belt.Option.getWithDefault(Ethers.BigInt.zero),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module SignatureAggregatorChanged = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.SignatureAggregatorChangedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.SignatureAggregatorChanged.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_SignatureAggregatorChangedWithContext(event, context),
      ~eventName=Types.EntryPoint_SignatureAggregatorChanged,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.SignatureAggregatorChangedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.SignatureAggregatorChanged.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_SignatureAggregatorChangedWithContext(event, context),
      ~eventName=Types.EntryPoint_SignatureAggregatorChanged,
    )

    @genType
    type createMockArgs = {
      aggregator?: Ethers.ethAddress,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {?aggregator, ?mockEventData} = args

      let params: Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs = {
        aggregator: aggregator->Belt.Option.getWithDefault(
          TestHelpers_MockAddresses.defaultAddress,
        ),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module StakeLocked = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.StakeLockedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.StakeLocked.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_StakeLockedWithContext(event, context),
      ~eventName=Types.EntryPoint_StakeLocked,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.StakeLockedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.StakeLocked.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_StakeLockedWithContext(event, context),
      ~eventName=Types.EntryPoint_StakeLocked,
    )

    @genType
    type createMockArgs = {
      account?: Ethers.ethAddress,
      totalStaked?: Ethers.BigInt.t,
      unstakeDelaySec?: Ethers.BigInt.t,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {?account, ?totalStaked, ?unstakeDelaySec, ?mockEventData} = args

      let params: Types.EntryPointContract.StakeLockedEvent.eventArgs = {
        account: account->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        totalStaked: totalStaked->Belt.Option.getWithDefault(Ethers.BigInt.zero),
        unstakeDelaySec: unstakeDelaySec->Belt.Option.getWithDefault(Ethers.BigInt.zero),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module StakeUnlocked = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.StakeUnlockedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.StakeUnlocked.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_StakeUnlockedWithContext(event, context),
      ~eventName=Types.EntryPoint_StakeUnlocked,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.StakeUnlockedEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.StakeUnlocked.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_StakeUnlockedWithContext(event, context),
      ~eventName=Types.EntryPoint_StakeUnlocked,
    )

    @genType
    type createMockArgs = {
      account?: Ethers.ethAddress,
      withdrawTime?: Ethers.BigInt.t,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {?account, ?withdrawTime, ?mockEventData} = args

      let params: Types.EntryPointContract.StakeUnlockedEvent.eventArgs = {
        account: account->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        withdrawTime: withdrawTime->Belt.Option.getWithDefault(Ethers.BigInt.zero),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module StakeWithdrawn = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.StakeWithdrawnEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.StakeWithdrawn.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_StakeWithdrawnWithContext(event, context),
      ~eventName=Types.EntryPoint_StakeWithdrawn,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.StakeWithdrawnEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.StakeWithdrawn.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_StakeWithdrawnWithContext(event, context),
      ~eventName=Types.EntryPoint_StakeWithdrawn,
    )

    @genType
    type createMockArgs = {
      account?: Ethers.ethAddress,
      withdrawAddress?: Ethers.ethAddress,
      amount?: Ethers.BigInt.t,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {?account, ?withdrawAddress, ?amount, ?mockEventData} = args

      let params: Types.EntryPointContract.StakeWithdrawnEvent.eventArgs = {
        account: account->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        withdrawAddress: withdrawAddress->Belt.Option.getWithDefault(
          TestHelpers_MockAddresses.defaultAddress,
        ),
        amount: amount->Belt.Option.getWithDefault(Ethers.BigInt.zero),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module UserOperationEvent = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.UserOperationEventEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.UserOperationEvent.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_UserOperationEventWithContext(event, context),
      ~eventName=Types.EntryPoint_UserOperationEvent,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.UserOperationEventEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.UserOperationEvent.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_UserOperationEventWithContext(event, context),
      ~eventName=Types.EntryPoint_UserOperationEvent,
    )

    @genType
    type createMockArgs = {
      userOpHash?: string,
      sender?: Ethers.ethAddress,
      paymaster?: Ethers.ethAddress,
      nonce?: Ethers.BigInt.t,
      success?: bool,
      actualGasCost?: Ethers.BigInt.t,
      actualGasUsed?: Ethers.BigInt.t,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {
        ?userOpHash,
        ?sender,
        ?paymaster,
        ?nonce,
        ?success,
        ?actualGasCost,
        ?actualGasUsed,
        ?mockEventData,
      } = args

      let params: Types.EntryPointContract.UserOperationEventEvent.eventArgs = {
        userOpHash: userOpHash->Belt.Option.getWithDefault("foo"),
        sender: sender->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        paymaster: paymaster->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        nonce: nonce->Belt.Option.getWithDefault(Ethers.BigInt.zero),
        success: success->Belt.Option.getWithDefault(false),
        actualGasCost: actualGasCost->Belt.Option.getWithDefault(Ethers.BigInt.zero),
        actualGasUsed: actualGasUsed->Belt.Option.getWithDefault(Ethers.BigInt.zero),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module UserOperationRevertReason = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.UserOperationRevertReasonEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.UserOperationRevertReason.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_UserOperationRevertReasonWithContext(event, context),
      ~eventName=Types.EntryPoint_UserOperationRevertReason,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.UserOperationRevertReasonEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.UserOperationRevertReason.getLoader,
      ~eventWithContextAccessor=(
        event,
        context,
      ) => Context.EntryPointContract_UserOperationRevertReasonWithContext(event, context),
      ~eventName=Types.EntryPoint_UserOperationRevertReason,
    )

    @genType
    type createMockArgs = {
      userOpHash?: string,
      sender?: Ethers.ethAddress,
      nonce?: Ethers.BigInt.t,
      revertReason?: string,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {?userOpHash, ?sender, ?nonce, ?revertReason, ?mockEventData} = args

      let params: Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs = {
        userOpHash: userOpHash->Belt.Option.getWithDefault("foo"),
        sender: sender->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        nonce: nonce->Belt.Option.getWithDefault(Ethers.BigInt.zero),
        revertReason: revertReason->Belt.Option.getWithDefault("foo"),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }

  module Withdrawn = {
    @genType
    let processEvent = EventFunctions.makeSyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.WithdrawnEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.Withdrawn.getLoader,
      ~eventWithContextAccessor=(event, context) => Context.EntryPointContract_WithdrawnWithContext(
        event,
        context,
      ),
      ~eventName=Types.EntryPoint_Withdrawn,
    )

    @genType
    let processEventAsync = EventFunctions.makeAsyncEventProcessor(
      ~contextCreator=Context.EntryPointContract.WithdrawnEvent.contextCreator,
      ~getLoader=Handlers.EntryPointContract.Withdrawn.getLoader,
      ~eventWithContextAccessor=(event, context) => Context.EntryPointContract_WithdrawnWithContext(
        event,
        context,
      ),
      ~eventName=Types.EntryPoint_Withdrawn,
    )

    @genType
    type createMockArgs = {
      account?: Ethers.ethAddress,
      withdrawAddress?: Ethers.ethAddress,
      amount?: Ethers.BigInt.t,
      mockEventData?: EventFunctions.mockEventData,
    }

    @genType
    let createMockEvent = args => {
      let {?account, ?withdrawAddress, ?amount, ?mockEventData} = args

      let params: Types.EntryPointContract.WithdrawnEvent.eventArgs = {
        account: account->Belt.Option.getWithDefault(TestHelpers_MockAddresses.defaultAddress),
        withdrawAddress: withdrawAddress->Belt.Option.getWithDefault(
          TestHelpers_MockAddresses.defaultAddress,
        ),
        amount: amount->Belt.Option.getWithDefault(Ethers.BigInt.zero),
      }

      EventFunctions.makeEventMocker(~params, ~mockEventData)
    }
  }
}
