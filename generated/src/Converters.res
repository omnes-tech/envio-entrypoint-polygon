exception UndefinedEvent(string)
let eventStringToEvent = (eventName: string, contractName: string): Types.eventName => {
  switch (eventName, contractName) {
  | ("AccountDeployed", "EntryPoint") => EntryPoint_AccountDeployed
  | ("BeforeExecution", "EntryPoint") => EntryPoint_BeforeExecution
  | ("Deposited", "EntryPoint") => EntryPoint_Deposited
  | ("SignatureAggregatorChanged", "EntryPoint") => EntryPoint_SignatureAggregatorChanged
  | ("StakeLocked", "EntryPoint") => EntryPoint_StakeLocked
  | ("StakeUnlocked", "EntryPoint") => EntryPoint_StakeUnlocked
  | ("StakeWithdrawn", "EntryPoint") => EntryPoint_StakeWithdrawn
  | ("UserOperationEvent", "EntryPoint") => EntryPoint_UserOperationEvent
  | ("UserOperationRevertReason", "EntryPoint") => EntryPoint_UserOperationRevertReason
  | ("Withdrawn", "EntryPoint") => EntryPoint_Withdrawn
  | _ => UndefinedEvent(eventName)->raise
  }
}

module EntryPoint = {
  let convertAccountDeployedViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.AccountDeployedEvent.eventArgs,
  > = Obj.magic

  let convertAccountDeployedLogDescription = (
    log: Ethers.logDescription<'a>,
  ): Ethers.logDescription<Types.EntryPointContract.AccountDeployedEvent.eventArgs> => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<
      Types.EntryPointContract.AccountDeployedEvent.ethersEventArgs,
    > =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        userOpHash: args.userOpHash,
        sender: args.sender,
        factory: args.factory,
        paymaster: args.paymaster,
      },
    }
  }

  let convertAccountDeployedLog = (
    logDescription: Ethers.logDescription<Types.EntryPointContract.AccountDeployedEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.AccountDeployedEvent.eventArgs = {
      userOpHash: logDescription.args.userOpHash,
      sender: logDescription.args.sender,
      factory: logDescription.args.factory,
      paymaster: logDescription.args.paymaster,
    }

    let accountDeployedLog: Types.eventLog<
      Types.EntryPointContract.AccountDeployedEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_AccountDeployed(accountDeployedLog)
  }
  let convertAccountDeployedLogViem = (
    decodedEvent: Viem.decodedEvent<Types.EntryPointContract.AccountDeployedEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.AccountDeployedEvent.eventArgs = {
      userOpHash: decodedEvent.args.userOpHash,
      sender: decodedEvent.args.sender,
      factory: decodedEvent.args.factory,
      paymaster: decodedEvent.args.paymaster,
    }

    let accountDeployedLog: Types.eventLog<
      Types.EntryPointContract.AccountDeployedEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_AccountDeployed(accountDeployedLog)
  }

  let convertAccountDeployedDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.AccountDeployedEvent.eventArgs => {
    open Belt
    let fields = ["userOpHash", "sender", "factory", "paymaster"]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
  let convertBeforeExecutionViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.BeforeExecutionEvent.eventArgs,
  > = Obj.magic

  @warning("-27")
  let convertBeforeExecutionLogDescription = (
    log: Ethers.logDescription<'a>,
  ): Ethers.logDescription<Types.EntryPointContract.BeforeExecutionEvent.eventArgs> => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<
      Types.EntryPointContract.BeforeExecutionEvent.ethersEventArgs,
    > =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: (),
    }
  }

  @warning("-27")
  let convertBeforeExecutionLog = (
    logDescription: Ethers.logDescription<Types.EntryPointContract.BeforeExecutionEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.BeforeExecutionEvent.eventArgs = ()

    let beforeExecutionLog: Types.eventLog<
      Types.EntryPointContract.BeforeExecutionEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_BeforeExecution(beforeExecutionLog)
  }
  @warning("-27")
  let convertBeforeExecutionLogViem = (
    decodedEvent: Viem.decodedEvent<Types.EntryPointContract.BeforeExecutionEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.BeforeExecutionEvent.eventArgs = ()

    let beforeExecutionLog: Types.eventLog<
      Types.EntryPointContract.BeforeExecutionEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_BeforeExecution(beforeExecutionLog)
  }

  let convertBeforeExecutionDecodedEventParams = (
    _decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.BeforeExecutionEvent.eventArgs => {
    ()
  }
  let convertDepositedViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.DepositedEvent.eventArgs,
  > = Obj.magic

  let convertDepositedLogDescription = (log: Ethers.logDescription<'a>): Ethers.logDescription<
    Types.EntryPointContract.DepositedEvent.eventArgs,
  > => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<Types.EntryPointContract.DepositedEvent.ethersEventArgs> =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        account: args.account,
        totalDeposit: args.totalDeposit,
      },
    }
  }

  let convertDepositedLog = (
    logDescription: Ethers.logDescription<Types.EntryPointContract.DepositedEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.DepositedEvent.eventArgs = {
      account: logDescription.args.account,
      totalDeposit: logDescription.args.totalDeposit,
    }

    let depositedLog: Types.eventLog<Types.EntryPointContract.DepositedEvent.eventArgs> = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_Deposited(depositedLog)
  }
  let convertDepositedLogViem = (
    decodedEvent: Viem.decodedEvent<Types.EntryPointContract.DepositedEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.DepositedEvent.eventArgs = {
      account: decodedEvent.args.account,
      totalDeposit: decodedEvent.args.totalDeposit,
    }

    let depositedLog: Types.eventLog<Types.EntryPointContract.DepositedEvent.eventArgs> = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_Deposited(depositedLog)
  }

  let convertDepositedDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.DepositedEvent.eventArgs => {
    open Belt
    let fields = ["account", "totalDeposit"]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
  let convertSignatureAggregatorChangedViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs,
  > = Obj.magic

  let convertSignatureAggregatorChangedLogDescription = (
    log: Ethers.logDescription<'a>,
  ): Ethers.logDescription<Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs> => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<
      Types.EntryPointContract.SignatureAggregatorChangedEvent.ethersEventArgs,
    > =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        aggregator: args.aggregator,
      },
    }
  }

  let convertSignatureAggregatorChangedLog = (
    logDescription: Ethers.logDescription<
      Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs,
    >,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs = {
      aggregator: logDescription.args.aggregator,
    }

    let signatureAggregatorChangedLog: Types.eventLog<
      Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_SignatureAggregatorChanged(signatureAggregatorChangedLog)
  }
  let convertSignatureAggregatorChangedLogViem = (
    decodedEvent: Viem.decodedEvent<
      Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs,
    >,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs = {
      aggregator: decodedEvent.args.aggregator,
    }

    let signatureAggregatorChangedLog: Types.eventLog<
      Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_SignatureAggregatorChanged(signatureAggregatorChangedLog)
  }

  let convertSignatureAggregatorChangedDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs => {
    open Belt
    let fields = ["aggregator"]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
  let convertStakeLockedViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.StakeLockedEvent.eventArgs,
  > = Obj.magic

  let convertStakeLockedLogDescription = (log: Ethers.logDescription<'a>): Ethers.logDescription<
    Types.EntryPointContract.StakeLockedEvent.eventArgs,
  > => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<
      Types.EntryPointContract.StakeLockedEvent.ethersEventArgs,
    > =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        account: args.account,
        totalStaked: args.totalStaked,
        unstakeDelaySec: args.unstakeDelaySec,
      },
    }
  }

  let convertStakeLockedLog = (
    logDescription: Ethers.logDescription<Types.EntryPointContract.StakeLockedEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.StakeLockedEvent.eventArgs = {
      account: logDescription.args.account,
      totalStaked: logDescription.args.totalStaked,
      unstakeDelaySec: logDescription.args.unstakeDelaySec,
    }

    let stakeLockedLog: Types.eventLog<Types.EntryPointContract.StakeLockedEvent.eventArgs> = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_StakeLocked(stakeLockedLog)
  }
  let convertStakeLockedLogViem = (
    decodedEvent: Viem.decodedEvent<Types.EntryPointContract.StakeLockedEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.StakeLockedEvent.eventArgs = {
      account: decodedEvent.args.account,
      totalStaked: decodedEvent.args.totalStaked,
      unstakeDelaySec: decodedEvent.args.unstakeDelaySec,
    }

    let stakeLockedLog: Types.eventLog<Types.EntryPointContract.StakeLockedEvent.eventArgs> = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_StakeLocked(stakeLockedLog)
  }

  let convertStakeLockedDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.StakeLockedEvent.eventArgs => {
    open Belt
    let fields = ["account", "totalStaked", "unstakeDelaySec"]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
  let convertStakeUnlockedViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.StakeUnlockedEvent.eventArgs,
  > = Obj.magic

  let convertStakeUnlockedLogDescription = (log: Ethers.logDescription<'a>): Ethers.logDescription<
    Types.EntryPointContract.StakeUnlockedEvent.eventArgs,
  > => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<
      Types.EntryPointContract.StakeUnlockedEvent.ethersEventArgs,
    > =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        account: args.account,
        withdrawTime: args.withdrawTime,
      },
    }
  }

  let convertStakeUnlockedLog = (
    logDescription: Ethers.logDescription<Types.EntryPointContract.StakeUnlockedEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.StakeUnlockedEvent.eventArgs = {
      account: logDescription.args.account,
      withdrawTime: logDescription.args.withdrawTime,
    }

    let stakeUnlockedLog: Types.eventLog<Types.EntryPointContract.StakeUnlockedEvent.eventArgs> = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_StakeUnlocked(stakeUnlockedLog)
  }
  let convertStakeUnlockedLogViem = (
    decodedEvent: Viem.decodedEvent<Types.EntryPointContract.StakeUnlockedEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.StakeUnlockedEvent.eventArgs = {
      account: decodedEvent.args.account,
      withdrawTime: decodedEvent.args.withdrawTime,
    }

    let stakeUnlockedLog: Types.eventLog<Types.EntryPointContract.StakeUnlockedEvent.eventArgs> = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_StakeUnlocked(stakeUnlockedLog)
  }

  let convertStakeUnlockedDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.StakeUnlockedEvent.eventArgs => {
    open Belt
    let fields = ["account", "withdrawTime"]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
  let convertStakeWithdrawnViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.StakeWithdrawnEvent.eventArgs,
  > = Obj.magic

  let convertStakeWithdrawnLogDescription = (log: Ethers.logDescription<'a>): Ethers.logDescription<
    Types.EntryPointContract.StakeWithdrawnEvent.eventArgs,
  > => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<
      Types.EntryPointContract.StakeWithdrawnEvent.ethersEventArgs,
    > =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        account: args.account,
        withdrawAddress: args.withdrawAddress,
        amount: args.amount,
      },
    }
  }

  let convertStakeWithdrawnLog = (
    logDescription: Ethers.logDescription<Types.EntryPointContract.StakeWithdrawnEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.StakeWithdrawnEvent.eventArgs = {
      account: logDescription.args.account,
      withdrawAddress: logDescription.args.withdrawAddress,
      amount: logDescription.args.amount,
    }

    let stakeWithdrawnLog: Types.eventLog<
      Types.EntryPointContract.StakeWithdrawnEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_StakeWithdrawn(stakeWithdrawnLog)
  }
  let convertStakeWithdrawnLogViem = (
    decodedEvent: Viem.decodedEvent<Types.EntryPointContract.StakeWithdrawnEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.StakeWithdrawnEvent.eventArgs = {
      account: decodedEvent.args.account,
      withdrawAddress: decodedEvent.args.withdrawAddress,
      amount: decodedEvent.args.amount,
    }

    let stakeWithdrawnLog: Types.eventLog<
      Types.EntryPointContract.StakeWithdrawnEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_StakeWithdrawn(stakeWithdrawnLog)
  }

  let convertStakeWithdrawnDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.StakeWithdrawnEvent.eventArgs => {
    open Belt
    let fields = ["account", "withdrawAddress", "amount"]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
  let convertUserOperationEventViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.UserOperationEventEvent.eventArgs,
  > = Obj.magic

  let convertUserOperationEventLogDescription = (
    log: Ethers.logDescription<'a>,
  ): Ethers.logDescription<Types.EntryPointContract.UserOperationEventEvent.eventArgs> => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<
      Types.EntryPointContract.UserOperationEventEvent.ethersEventArgs,
    > =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        userOpHash: args.userOpHash,
        sender: args.sender,
        paymaster: args.paymaster,
        nonce: args.nonce,
        success: args.success,
        actualGasCost: args.actualGasCost,
        actualGasUsed: args.actualGasUsed,
      },
    }
  }

  let convertUserOperationEventLog = (
    logDescription: Ethers.logDescription<
      Types.EntryPointContract.UserOperationEventEvent.eventArgs,
    >,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.UserOperationEventEvent.eventArgs = {
      userOpHash: logDescription.args.userOpHash,
      sender: logDescription.args.sender,
      paymaster: logDescription.args.paymaster,
      nonce: logDescription.args.nonce,
      success: logDescription.args.success,
      actualGasCost: logDescription.args.actualGasCost,
      actualGasUsed: logDescription.args.actualGasUsed,
    }

    let userOperationEventLog: Types.eventLog<
      Types.EntryPointContract.UserOperationEventEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_UserOperationEvent(userOperationEventLog)
  }
  let convertUserOperationEventLogViem = (
    decodedEvent: Viem.decodedEvent<Types.EntryPointContract.UserOperationEventEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.UserOperationEventEvent.eventArgs = {
      userOpHash: decodedEvent.args.userOpHash,
      sender: decodedEvent.args.sender,
      paymaster: decodedEvent.args.paymaster,
      nonce: decodedEvent.args.nonce,
      success: decodedEvent.args.success,
      actualGasCost: decodedEvent.args.actualGasCost,
      actualGasUsed: decodedEvent.args.actualGasUsed,
    }

    let userOperationEventLog: Types.eventLog<
      Types.EntryPointContract.UserOperationEventEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_UserOperationEvent(userOperationEventLog)
  }

  let convertUserOperationEventDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.UserOperationEventEvent.eventArgs => {
    open Belt
    let fields = [
      "userOpHash",
      "sender",
      "paymaster",
      "nonce",
      "success",
      "actualGasCost",
      "actualGasUsed",
    ]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
  let convertUserOperationRevertReasonViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs,
  > = Obj.magic

  let convertUserOperationRevertReasonLogDescription = (
    log: Ethers.logDescription<'a>,
  ): Ethers.logDescription<Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs> => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<
      Types.EntryPointContract.UserOperationRevertReasonEvent.ethersEventArgs,
    > =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        userOpHash: args.userOpHash,
        sender: args.sender,
        nonce: args.nonce,
        revertReason: args.revertReason,
      },
    }
  }

  let convertUserOperationRevertReasonLog = (
    logDescription: Ethers.logDescription<
      Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs,
    >,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs = {
      userOpHash: logDescription.args.userOpHash,
      sender: logDescription.args.sender,
      nonce: logDescription.args.nonce,
      revertReason: logDescription.args.revertReason,
    }

    let userOperationRevertReasonLog: Types.eventLog<
      Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_UserOperationRevertReason(userOperationRevertReasonLog)
  }
  let convertUserOperationRevertReasonLogViem = (
    decodedEvent: Viem.decodedEvent<
      Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs,
    >,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs = {
      userOpHash: decodedEvent.args.userOpHash,
      sender: decodedEvent.args.sender,
      nonce: decodedEvent.args.nonce,
      revertReason: decodedEvent.args.revertReason,
    }

    let userOperationRevertReasonLog: Types.eventLog<
      Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs,
    > = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_UserOperationRevertReason(userOperationRevertReasonLog)
  }

  let convertUserOperationRevertReasonDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs => {
    open Belt
    let fields = ["userOpHash", "sender", "nonce", "revertReason"]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
  let convertWithdrawnViemDecodedEvent: Viem.decodedEvent<'a> => Viem.decodedEvent<
    Types.EntryPointContract.WithdrawnEvent.eventArgs,
  > = Obj.magic

  let convertWithdrawnLogDescription = (log: Ethers.logDescription<'a>): Ethers.logDescription<
    Types.EntryPointContract.WithdrawnEvent.eventArgs,
  > => {
    //Convert from the ethersLog type with indexs as keys to named key value object
    let ethersLog: Ethers.logDescription<Types.EntryPointContract.WithdrawnEvent.ethersEventArgs> =
      log->Obj.magic
    let {args, name, signature, topic} = ethersLog

    {
      name,
      signature,
      topic,
      args: {
        account: args.account,
        withdrawAddress: args.withdrawAddress,
        amount: args.amount,
      },
    }
  }

  let convertWithdrawnLog = (
    logDescription: Ethers.logDescription<Types.EntryPointContract.WithdrawnEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.WithdrawnEvent.eventArgs = {
      account: logDescription.args.account,
      withdrawAddress: logDescription.args.withdrawAddress,
      amount: logDescription.args.amount,
    }

    let withdrawnLog: Types.eventLog<Types.EntryPointContract.WithdrawnEvent.eventArgs> = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_Withdrawn(withdrawnLog)
  }
  let convertWithdrawnLogViem = (
    decodedEvent: Viem.decodedEvent<Types.EntryPointContract.WithdrawnEvent.eventArgs>,
    ~log: Ethers.log,
    ~blockTimestamp: int,
    ~chainId: int,
    ~txOrigin: option<Ethers.ethAddress>,
    ~txTo: option<Ethers.ethAddress>,
  ) => {
    let params: Types.EntryPointContract.WithdrawnEvent.eventArgs = {
      account: decodedEvent.args.account,
      withdrawAddress: decodedEvent.args.withdrawAddress,
      amount: decodedEvent.args.amount,
    }

    let withdrawnLog: Types.eventLog<Types.EntryPointContract.WithdrawnEvent.eventArgs> = {
      params,
      chainId,
      txOrigin,
      txTo,
      blockNumber: log.blockNumber,
      blockTimestamp,
      blockHash: log.blockHash,
      srcAddress: log.address,
      transactionHash: log.transactionHash,
      transactionIndex: log.transactionIndex,
      logIndex: log.logIndex,
    }

    Types.EntryPointContract_Withdrawn(withdrawnLog)
  }

  let convertWithdrawnDecodedEventParams = (
    decodedEvent: HyperSyncClient.Decoder.decodedEvent,
  ): Types.EntryPointContract.WithdrawnEvent.eventArgs => {
    open Belt
    let fields = ["account", "withdrawAddress", "amount"]
    let values =
      Array.concat(decodedEvent.indexed, decodedEvent.body)->Array.map(
        HyperSyncClient.Decoder.toUnderlying,
      )
    Array.zip(fields, values)->Js.Dict.fromArray->Obj.magic
  }
}

exception ParseError(Ethers.Interface.parseLogError)
exception UnregisteredContract(Ethers.ethAddress)

let parseEventEthers = (
  ~log,
  ~blockTimestamp,
  ~contractInterfaceManager,
  ~chainId,
  ~txOrigin,
  ~txTo,
): Belt.Result.t<Types.event, _> => {
  let logDescriptionResult = contractInterfaceManager->ContractInterfaceManager.parseLogEthers(~log)
  switch logDescriptionResult {
  | Error(e) =>
    switch e {
    | ParseError(parseError) => ParseError(parseError)
    | UndefinedInterface(contractAddress) => UnregisteredContract(contractAddress)
    }->Error

  | Ok(logDescription) =>
    switch contractInterfaceManager->ContractInterfaceManager.getContractNameFromAddress(
      ~contractAddress=log.address,
    ) {
    | None => Error(UnregisteredContract(log.address))
    | Some(contractName) =>
      let event = switch eventStringToEvent(logDescription.name, contractName) {
      | EntryPoint_AccountDeployed =>
        logDescription
        ->EntryPoint.convertAccountDeployedLogDescription
        ->EntryPoint.convertAccountDeployedLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_BeforeExecution =>
        logDescription
        ->EntryPoint.convertBeforeExecutionLogDescription
        ->EntryPoint.convertBeforeExecutionLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_Deposited =>
        logDescription
        ->EntryPoint.convertDepositedLogDescription
        ->EntryPoint.convertDepositedLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_SignatureAggregatorChanged =>
        logDescription
        ->EntryPoint.convertSignatureAggregatorChangedLogDescription
        ->EntryPoint.convertSignatureAggregatorChangedLog(
          ~log,
          ~blockTimestamp,
          ~chainId,
          ~txOrigin,
          ~txTo,
        )
      | EntryPoint_StakeLocked =>
        logDescription
        ->EntryPoint.convertStakeLockedLogDescription
        ->EntryPoint.convertStakeLockedLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_StakeUnlocked =>
        logDescription
        ->EntryPoint.convertStakeUnlockedLogDescription
        ->EntryPoint.convertStakeUnlockedLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_StakeWithdrawn =>
        logDescription
        ->EntryPoint.convertStakeWithdrawnLogDescription
        ->EntryPoint.convertStakeWithdrawnLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_UserOperationEvent =>
        logDescription
        ->EntryPoint.convertUserOperationEventLogDescription
        ->EntryPoint.convertUserOperationEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_UserOperationRevertReason =>
        logDescription
        ->EntryPoint.convertUserOperationRevertReasonLogDescription
        ->EntryPoint.convertUserOperationRevertReasonLog(
          ~log,
          ~blockTimestamp,
          ~chainId,
          ~txOrigin,
          ~txTo,
        )
      | EntryPoint_Withdrawn =>
        logDescription
        ->EntryPoint.convertWithdrawnLogDescription
        ->EntryPoint.convertWithdrawnLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      }

      Ok(event)
    }
  }
}

let makeEventLog = (
  params: 'args,
  ~log: Ethers.log,
  ~blockTimestamp: int,
  ~chainId: int,
  ~txOrigin: option<Ethers.ethAddress>,
  ~txTo: option<Ethers.ethAddress>,
): Types.eventLog<'args> => {
  chainId,
  params,
  txOrigin,
  txTo,
  blockNumber: log.blockNumber,
  blockTimestamp,
  blockHash: log.blockHash,
  srcAddress: log.address,
  transactionHash: log.transactionHash,
  transactionIndex: log.transactionIndex,
  logIndex: log.logIndex,
}

let convertDecodedEvent = (
  event: HyperSyncClient.Decoder.decodedEvent,
  ~contractInterfaceManager,
  ~log: Ethers.log,
  ~blockTimestamp,
  ~chainId,
  ~txOrigin: option<Ethers.ethAddress>,
  ~txTo: option<Ethers.ethAddress>,
): result<Types.event, _> => {
  switch contractInterfaceManager->ContractInterfaceManager.getContractNameFromAddress(
    ~contractAddress=log.address,
  ) {
  | None => Error(UnregisteredContract(log.address))
  | Some(contractName) =>
    let event = switch Types.eventTopicToEventName(contractName, log.topics[0]) {
    | EntryPoint_AccountDeployed =>
      event
      ->EntryPoint.convertAccountDeployedDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_AccountDeployed
    | EntryPoint_BeforeExecution =>
      event
      ->EntryPoint.convertBeforeExecutionDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_BeforeExecution
    | EntryPoint_Deposited =>
      event
      ->EntryPoint.convertDepositedDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_Deposited
    | EntryPoint_SignatureAggregatorChanged =>
      event
      ->EntryPoint.convertSignatureAggregatorChangedDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_SignatureAggregatorChanged
    | EntryPoint_StakeLocked =>
      event
      ->EntryPoint.convertStakeLockedDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_StakeLocked
    | EntryPoint_StakeUnlocked =>
      event
      ->EntryPoint.convertStakeUnlockedDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_StakeUnlocked
    | EntryPoint_StakeWithdrawn =>
      event
      ->EntryPoint.convertStakeWithdrawnDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_StakeWithdrawn
    | EntryPoint_UserOperationEvent =>
      event
      ->EntryPoint.convertUserOperationEventDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_UserOperationEvent
    | EntryPoint_UserOperationRevertReason =>
      event
      ->EntryPoint.convertUserOperationRevertReasonDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_UserOperationRevertReason
    | EntryPoint_Withdrawn =>
      event
      ->EntryPoint.convertWithdrawnDecodedEventParams
      ->makeEventLog(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      ->Types.EntryPointContract_Withdrawn
    }
    Ok(event)
  }
}

let parseEvent = (
  ~log,
  ~blockTimestamp,
  ~contractInterfaceManager,
  ~chainId,
  ~txOrigin,
  ~txTo,
): Belt.Result.t<Types.event, _> => {
  let decodedEventResult = contractInterfaceManager->ContractInterfaceManager.parseLogViem(~log)
  switch decodedEventResult {
  | Error(e) =>
    switch e {
    | ParseError(parseError) => ParseError(parseError)
    | UndefinedInterface(contractAddress) => UnregisteredContract(contractAddress)
    }->Error

  | Ok(decodedEvent) =>
    switch contractInterfaceManager->ContractInterfaceManager.getContractNameFromAddress(
      ~contractAddress=log.address,
    ) {
    | None => Error(UnregisteredContract(log.address))
    | Some(contractName) =>
      let event = switch eventStringToEvent(decodedEvent.eventName, contractName) {
      | EntryPoint_AccountDeployed =>
        decodedEvent
        ->EntryPoint.convertAccountDeployedViemDecodedEvent
        ->EntryPoint.convertAccountDeployedLogViem(
          ~log,
          ~blockTimestamp,
          ~chainId,
          ~txOrigin,
          ~txTo,
        )
      | EntryPoint_BeforeExecution =>
        decodedEvent
        ->EntryPoint.convertBeforeExecutionViemDecodedEvent
        ->EntryPoint.convertBeforeExecutionLogViem(
          ~log,
          ~blockTimestamp,
          ~chainId,
          ~txOrigin,
          ~txTo,
        )
      | EntryPoint_Deposited =>
        decodedEvent
        ->EntryPoint.convertDepositedViemDecodedEvent
        ->EntryPoint.convertDepositedLogViem(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_SignatureAggregatorChanged =>
        decodedEvent
        ->EntryPoint.convertSignatureAggregatorChangedViemDecodedEvent
        ->EntryPoint.convertSignatureAggregatorChangedLogViem(
          ~log,
          ~blockTimestamp,
          ~chainId,
          ~txOrigin,
          ~txTo,
        )
      | EntryPoint_StakeLocked =>
        decodedEvent
        ->EntryPoint.convertStakeLockedViemDecodedEvent
        ->EntryPoint.convertStakeLockedLogViem(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_StakeUnlocked =>
        decodedEvent
        ->EntryPoint.convertStakeUnlockedViemDecodedEvent
        ->EntryPoint.convertStakeUnlockedLogViem(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_StakeWithdrawn =>
        decodedEvent
        ->EntryPoint.convertStakeWithdrawnViemDecodedEvent
        ->EntryPoint.convertStakeWithdrawnLogViem(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      | EntryPoint_UserOperationEvent =>
        decodedEvent
        ->EntryPoint.convertUserOperationEventViemDecodedEvent
        ->EntryPoint.convertUserOperationEventLogViem(
          ~log,
          ~blockTimestamp,
          ~chainId,
          ~txOrigin,
          ~txTo,
        )
      | EntryPoint_UserOperationRevertReason =>
        decodedEvent
        ->EntryPoint.convertUserOperationRevertReasonViemDecodedEvent
        ->EntryPoint.convertUserOperationRevertReasonLogViem(
          ~log,
          ~blockTimestamp,
          ~chainId,
          ~txOrigin,
          ~txTo,
        )
      | EntryPoint_Withdrawn =>
        decodedEvent
        ->EntryPoint.convertWithdrawnViemDecodedEvent
        ->EntryPoint.convertWithdrawnLogViem(~log, ~blockTimestamp, ~chainId, ~txOrigin, ~txTo)
      }

      Ok(event)
    }
  }
}

let decodeRawEventWith = (
  rawEvent: Types.rawEventsEntity,
  ~schema: S.t<'a>,
  ~variantAccessor: Types.eventLog<'a> => Types.event,
  ~chain,
  ~txOrigin: option<Ethers.ethAddress>,
  ~txTo: option<Ethers.ethAddress>,
): result<Types.eventBatchQueueItem, S.error> => {
  rawEvent.params
  ->S.parseJsonStringWith(schema)
  ->Belt.Result.map(params => {
    let event = {
      chainId: rawEvent.chainId,
      txOrigin,
      txTo,
      blockNumber: rawEvent.blockNumber,
      blockTimestamp: rawEvent.blockTimestamp,
      blockHash: rawEvent.blockHash,
      srcAddress: rawEvent.srcAddress,
      transactionHash: rawEvent.transactionHash,
      transactionIndex: rawEvent.transactionIndex,
      logIndex: rawEvent.logIndex,
      params,
    }->variantAccessor

    let queueItem: Types.eventBatchQueueItem = {
      timestamp: rawEvent.blockTimestamp,
      chain,
      blockNumber: rawEvent.blockNumber,
      logIndex: rawEvent.logIndex,
      event,
    }

    queueItem
  })
}

let parseRawEvent = (
  rawEvent: Types.rawEventsEntity,
  ~chain,
  ~txOrigin: option<Ethers.ethAddress>,
  ~txTo: option<Ethers.ethAddress>,
): result<Types.eventBatchQueueItem, S.error> => {
  rawEvent.eventType
  ->S.parseWith(Types.eventNameSchema)
  ->Belt.Result.flatMap(eventName => {
    switch eventName {
    | EntryPoint_AccountDeployed =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.AccountDeployedEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_AccountDeployed(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_BeforeExecution =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.BeforeExecutionEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_BeforeExecution(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_Deposited =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.DepositedEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_Deposited(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_SignatureAggregatorChanged =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_SignatureAggregatorChanged(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_StakeLocked =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.StakeLockedEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_StakeLocked(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_StakeUnlocked =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.StakeUnlockedEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_StakeUnlocked(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_StakeWithdrawn =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.StakeWithdrawnEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_StakeWithdrawn(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_UserOperationEvent =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.UserOperationEventEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_UserOperationEvent(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_UserOperationRevertReason =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_UserOperationRevertReason(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    | EntryPoint_Withdrawn =>
      rawEvent->decodeRawEventWith(
        ~schema=Types.EntryPointContract.WithdrawnEvent.eventArgsSchema,
        ~variantAccessor=event => Types.EntryPointContract_Withdrawn(event),
        ~chain,
        ~txOrigin,
        ~txTo,
      )
    }
  })
}
