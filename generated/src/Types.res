//*************
//***ENTITIES**
//*************
@genType.as("Id")
type id = string

@@warning("-30")
@genType
type rec entryPoint_AccountDeployedLoaderConfig = bool
and entryPoint_BeforeExecutionLoaderConfig = bool
and entryPoint_DepositedLoaderConfig = bool
and entryPoint_SignatureAggregatorChangedLoaderConfig = bool
and entryPoint_StakeLockedLoaderConfig = bool
and entryPoint_StakeUnlockedLoaderConfig = bool
and entryPoint_StakeWithdrawnLoaderConfig = bool
and entryPoint_UserOperationEventLoaderConfig = bool
and entryPoint_UserOperationRevertReasonLoaderConfig = bool
and entryPoint_WithdrawnLoaderConfig = bool
@@warning("+30")

@genType
type entityRead =
  | EntryPoint_AccountDeployedRead(id)
  | EntryPoint_BeforeExecutionRead(id)
  | EntryPoint_DepositedRead(id)
  | EntryPoint_SignatureAggregatorChangedRead(id)
  | EntryPoint_StakeLockedRead(id)
  | EntryPoint_StakeUnlockedRead(id)
  | EntryPoint_StakeWithdrawnRead(id)
  | EntryPoint_UserOperationEventRead(id)
  | EntryPoint_UserOperationRevertReasonRead(id)
  | EntryPoint_WithdrawnRead(id)

@genType
type rawEventsEntity = {
  @as("chain_id") chainId: int,
  @as("event_id") eventId: string,
  @as("block_number") blockNumber: int,
  @as("log_index") logIndex: int,
  @as("transaction_index") transactionIndex: int,
  @as("transaction_hash") transactionHash: string,
  @as("src_address") srcAddress: Ethers.ethAddress,
  @as("block_hash") blockHash: string,
  @as("block_timestamp") blockTimestamp: int,
  @as("event_type") eventType: Js.Json.t,
  params: string,
}

@genType
type dynamicContractRegistryEntity = {
  @as("chain_id") chainId: int,
  @as("event_id") eventId: Ethers.BigInt.t,
  @as("block_timestamp") blockTimestamp: int,
  @as("contract_address") contractAddress: Ethers.ethAddress,
  @as("contract_type") contractType: string,
}

//Re-exporting types for backwards compatability
@genType.as("EntryPoint_AccountDeployedEntity")
type entryPoint_AccountDeployedEntity = Entities.EntryPoint_AccountDeployed.t
@genType.as("EntryPoint_BeforeExecutionEntity")
type entryPoint_BeforeExecutionEntity = Entities.EntryPoint_BeforeExecution.t
@genType.as("EntryPoint_DepositedEntity")
type entryPoint_DepositedEntity = Entities.EntryPoint_Deposited.t
@genType.as("EntryPoint_SignatureAggregatorChangedEntity")
type entryPoint_SignatureAggregatorChangedEntity = Entities.EntryPoint_SignatureAggregatorChanged.t
@genType.as("EntryPoint_StakeLockedEntity")
type entryPoint_StakeLockedEntity = Entities.EntryPoint_StakeLocked.t
@genType.as("EntryPoint_StakeUnlockedEntity")
type entryPoint_StakeUnlockedEntity = Entities.EntryPoint_StakeUnlocked.t
@genType.as("EntryPoint_StakeWithdrawnEntity")
type entryPoint_StakeWithdrawnEntity = Entities.EntryPoint_StakeWithdrawn.t
@genType.as("EntryPoint_UserOperationEventEntity")
type entryPoint_UserOperationEventEntity = Entities.EntryPoint_UserOperationEvent.t
@genType.as("EntryPoint_UserOperationRevertReasonEntity")
type entryPoint_UserOperationRevertReasonEntity = Entities.EntryPoint_UserOperationRevertReason.t
@genType.as("EntryPoint_WithdrawnEntity")
type entryPoint_WithdrawnEntity = Entities.EntryPoint_Withdrawn.t

type eventIdentifier = {
  chainId: int,
  blockTimestamp: int,
  blockNumber: int,
  logIndex: int,
}

type entityUpdateAction<'entityType> =
  | Set('entityType)
  | Delete(string)

type entityUpdate<'entityType> = {
  eventIdentifier: eventIdentifier,
  shouldSaveHistory: bool,
  entityUpdateAction: entityUpdateAction<'entityType>,
}

let mkEntityUpdate = (~shouldSaveHistory=true, ~eventIdentifier, entityUpdateAction) => {
  shouldSaveHistory,
  eventIdentifier,
  entityUpdateAction,
}

type entityValueAtStartOfBatch<'entityType> =
  | NotSet // The entity isn't in the DB yet
  | AlreadySet('entityType)

type existingValueInDb<'entityType> =
  | Retrieved(entityValueAtStartOfBatch<'entityType>)
  // NOTE: We use an postgres function solve the issue of this entities previous value not being known.
  | Unknown

type updatedValue<'entityType> = {
  // Initial value within a batch
  initial: existingValueInDb<'entityType>,
  latest: entityUpdate<'entityType>,
  history: array<entityUpdate<'entityType>>,
}
@genType
type inMemoryStoreRowEntity<'entityType> =
  | Updated(updatedValue<'entityType>)
  | InitialReadFromDb(entityValueAtStartOfBatch<'entityType>) // This means there is no change from the db.

@genType
type inMemoryStoreRowMeta<'a> = 'a

//*************
//**CONTRACTS**
//*************

@genType.as("EventLog")
type eventLog<'a> = {
  params: 'a,
  chainId: int,
  txOrigin: option<Ethers.ethAddress>,
  txTo: option<Ethers.ethAddress>,
  blockNumber: int,
  blockTimestamp: int,
  blockHash: string,
  srcAddress: Ethers.ethAddress,
  transactionHash: string,
  transactionIndex: int,
  logIndex: int,
}

module EntryPointContract = {
  module AccountDeployedEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {
      @as("0") userOpHash: string,
      @as("1") sender: Ethers.ethAddress,
      @as("2") factory: Ethers.ethAddress,
      @as("3") paymaster: Ethers.ethAddress,
    }

    @genType
    type eventArgs = {
      userOpHash: string,
      sender: Ethers.ethAddress,
      factory: Ethers.ethAddress,
      paymaster: Ethers.ethAddress,
    }
    let eventArgsSchema = S.object((. s) => {
      userOpHash: s.field("userOpHash", S.string),
      sender: s.field("sender", Ethers.ethAddressSchema),
      factory: s.field("factory", Ethers.ethAddressSchema),
      paymaster: s.field("paymaster", Ethers.ethAddressSchema),
    })

    @genType.as("EntryPointContract_AccountDeployed_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module BeforeExecutionEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = unit

    @genType
    type eventArgs = unit
    let eventArgsSchema = S.literal(%raw(`null`))->S.variant((. _) => ())

    @genType.as("EntryPointContract_BeforeExecution_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module DepositedEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {
      @as("0") account: Ethers.ethAddress,
      @as("1") totalDeposit: Ethers.BigInt.t,
    }

    @genType
    type eventArgs = {
      account: Ethers.ethAddress,
      totalDeposit: Ethers.BigInt.t,
    }
    let eventArgsSchema = S.object((. s) => {
      account: s.field("account", Ethers.ethAddressSchema),
      totalDeposit: s.field("totalDeposit", Ethers.BigInt.schema),
    })

    @genType.as("EntryPointContract_Deposited_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module SignatureAggregatorChangedEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {@as("0") aggregator: Ethers.ethAddress}

    @genType
    type eventArgs = {aggregator: Ethers.ethAddress}
    let eventArgsSchema = S.object((. s) => {
      aggregator: s.field("aggregator", Ethers.ethAddressSchema),
    })

    @genType.as("EntryPointContract_SignatureAggregatorChanged_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module StakeLockedEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {
      @as("0") account: Ethers.ethAddress,
      @as("1") totalStaked: Ethers.BigInt.t,
      @as("2") unstakeDelaySec: Ethers.BigInt.t,
    }

    @genType
    type eventArgs = {
      account: Ethers.ethAddress,
      totalStaked: Ethers.BigInt.t,
      unstakeDelaySec: Ethers.BigInt.t,
    }
    let eventArgsSchema = S.object((. s) => {
      account: s.field("account", Ethers.ethAddressSchema),
      totalStaked: s.field("totalStaked", Ethers.BigInt.schema),
      unstakeDelaySec: s.field("unstakeDelaySec", Ethers.BigInt.schema),
    })

    @genType.as("EntryPointContract_StakeLocked_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module StakeUnlockedEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {
      @as("0") account: Ethers.ethAddress,
      @as("1") withdrawTime: Ethers.BigInt.t,
    }

    @genType
    type eventArgs = {
      account: Ethers.ethAddress,
      withdrawTime: Ethers.BigInt.t,
    }
    let eventArgsSchema = S.object((. s) => {
      account: s.field("account", Ethers.ethAddressSchema),
      withdrawTime: s.field("withdrawTime", Ethers.BigInt.schema),
    })

    @genType.as("EntryPointContract_StakeUnlocked_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module StakeWithdrawnEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {
      @as("0") account: Ethers.ethAddress,
      @as("1") withdrawAddress: Ethers.ethAddress,
      @as("2") amount: Ethers.BigInt.t,
    }

    @genType
    type eventArgs = {
      account: Ethers.ethAddress,
      withdrawAddress: Ethers.ethAddress,
      amount: Ethers.BigInt.t,
    }
    let eventArgsSchema = S.object((. s) => {
      account: s.field("account", Ethers.ethAddressSchema),
      withdrawAddress: s.field("withdrawAddress", Ethers.ethAddressSchema),
      amount: s.field("amount", Ethers.BigInt.schema),
    })

    @genType.as("EntryPointContract_StakeWithdrawn_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module UserOperationEventEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {
      @as("0") userOpHash: string,
      @as("1") sender: Ethers.ethAddress,
      @as("2") paymaster: Ethers.ethAddress,
      @as("3") nonce: Ethers.BigInt.t,
      @as("4") success: bool,
      @as("5") actualGasCost: Ethers.BigInt.t,
      @as("6") actualGasUsed: Ethers.BigInt.t,
    }

    @genType
    type eventArgs = {
      userOpHash: string,
      sender: Ethers.ethAddress,
      paymaster: Ethers.ethAddress,
      nonce: Ethers.BigInt.t,
      success: bool,
      actualGasCost: Ethers.BigInt.t,
      actualGasUsed: Ethers.BigInt.t,
    }
    let eventArgsSchema = S.object((. s) => {
      userOpHash: s.field("userOpHash", S.string),
      sender: s.field("sender", Ethers.ethAddressSchema),
      paymaster: s.field("paymaster", Ethers.ethAddressSchema),
      nonce: s.field("nonce", Ethers.BigInt.schema),
      success: s.field("success", S.bool),
      actualGasCost: s.field("actualGasCost", Ethers.BigInt.schema),
      actualGasUsed: s.field("actualGasUsed", Ethers.BigInt.schema),
    })

    @genType.as("EntryPointContract_UserOperationEvent_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module UserOperationRevertReasonEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {
      @as("0") userOpHash: string,
      @as("1") sender: Ethers.ethAddress,
      @as("2") nonce: Ethers.BigInt.t,
      @as("3") revertReason: string,
    }

    @genType
    type eventArgs = {
      userOpHash: string,
      sender: Ethers.ethAddress,
      nonce: Ethers.BigInt.t,
      revertReason: string,
    }
    let eventArgsSchema = S.object((. s) => {
      userOpHash: s.field("userOpHash", S.string),
      sender: s.field("sender", Ethers.ethAddressSchema),
      nonce: s.field("nonce", Ethers.BigInt.schema),
      revertReason: s.field("revertReason", S.string),
    })

    @genType.as("EntryPointContract_UserOperationRevertReason_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
  module WithdrawnEvent = {
    //Note: each parameter is using a binding of its index to help with binding in ethers
    //This handles both unamed params and also named params that clash with reserved keywords
    //eg. if an event param is called "values" it will clash since eventArgs will have a '.values()' iterator
    type ethersEventArgs = {
      @as("0") account: Ethers.ethAddress,
      @as("1") withdrawAddress: Ethers.ethAddress,
      @as("2") amount: Ethers.BigInt.t,
    }

    @genType
    type eventArgs = {
      account: Ethers.ethAddress,
      withdrawAddress: Ethers.ethAddress,
      amount: Ethers.BigInt.t,
    }
    let eventArgsSchema = S.object((. s) => {
      account: s.field("account", Ethers.ethAddressSchema),
      withdrawAddress: s.field("withdrawAddress", Ethers.ethAddressSchema),
      amount: s.field("amount", Ethers.BigInt.schema),
    })

    @genType.as("EntryPointContract_Withdrawn_EventLog")
    type log = eventLog<eventArgs>

    // Entity: EntryPoint_AccountDeployed
    type entryPoint_AccountDeployedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_AccountDeployed.t>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_AccountDeployedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_AccountDeployed.t>>,
      set: Entities.EntryPoint_AccountDeployed.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_BeforeExecution
    type entryPoint_BeforeExecutionEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_BeforeExecution.t>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_BeforeExecutionEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_BeforeExecution.t>>,
      set: Entities.EntryPoint_BeforeExecution.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Deposited
    type entryPoint_DepositedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Deposited.t>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_DepositedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Deposited.t>>,
      set: Entities.EntryPoint_Deposited.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_SignatureAggregatorChanged
    type entryPoint_SignatureAggregatorChangedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_SignatureAggregatorChanged.t>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_SignatureAggregatorChanged.t>>,
      set: Entities.EntryPoint_SignatureAggregatorChanged.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeLocked
    type entryPoint_StakeLockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeLocked.t>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeLockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeLocked.t>>,
      set: Entities.EntryPoint_StakeLocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeUnlocked
    type entryPoint_StakeUnlockedEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeUnlocked.t>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeUnlockedEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeUnlocked.t>>,
      set: Entities.EntryPoint_StakeUnlocked.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_StakeWithdrawn
    type entryPoint_StakeWithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_StakeWithdrawn.t>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_StakeWithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_StakeWithdrawn.t>>,
      set: Entities.EntryPoint_StakeWithdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationEvent
    type entryPoint_UserOperationEventEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationEvent.t>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationEventEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationEvent.t>>,
      set: Entities.EntryPoint_UserOperationEvent.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_UserOperationRevertReason
    type entryPoint_UserOperationRevertReasonEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_UserOperationRevertReason.t>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_UserOperationRevertReasonEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_UserOperationRevertReason.t>>,
      set: Entities.EntryPoint_UserOperationRevertReason.t => unit,
      deleteUnsafe: id => unit,
    }

    // Entity: EntryPoint_Withdrawn
    type entryPoint_WithdrawnEntityHandlerContext = {
      get: id => option<Entities.EntryPoint_Withdrawn.t>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    type entryPoint_WithdrawnEntityHandlerContextAsync = {
      get: id => promise<option<Entities.EntryPoint_Withdrawn.t>>,
      set: Entities.EntryPoint_Withdrawn.t => unit,
      deleteUnsafe: id => unit,
    }

    @genType
    type handlerContext = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityHandlerContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContext,
    }
    @genType
    type handlerContextAsync = {
      log: Logs.userLogger,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityHandlerContextAsync,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityHandlerContextAsync,
      @as("EntryPoint_Deposited")
      entryPoint_Deposited: entryPoint_DepositedEntityHandlerContextAsync,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityHandlerContextAsync,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityHandlerContextAsync,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityHandlerContextAsync,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityHandlerContextAsync,
      @as("EntryPoint_Withdrawn")
      entryPoint_Withdrawn: entryPoint_WithdrawnEntityHandlerContextAsync,
    }

    @genType
    type entryPoint_AccountDeployedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_BeforeExecutionEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_DepositedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_SignatureAggregatorChangedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeLockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeUnlockedEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_StakeWithdrawnEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationEventEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_UserOperationRevertReasonEntityLoaderContext = {load: id => unit}
    @genType
    type entryPoint_WithdrawnEntityLoaderContext = {load: id => unit}

    @genType
    type contractRegistrations = {
      //TODO only add contracts we've registered for the event in the config
      addEntryPoint: Ethers.ethAddress => unit,
    }
    @genType
    type loaderContext = {
      log: Logs.userLogger,
      contractRegistration: contractRegistrations,
      @as("EntryPoint_AccountDeployed")
      entryPoint_AccountDeployed: entryPoint_AccountDeployedEntityLoaderContext,
      @as("EntryPoint_BeforeExecution")
      entryPoint_BeforeExecution: entryPoint_BeforeExecutionEntityLoaderContext,
      @as("EntryPoint_Deposited") entryPoint_Deposited: entryPoint_DepositedEntityLoaderContext,
      @as("EntryPoint_SignatureAggregatorChanged")
      entryPoint_SignatureAggregatorChanged: entryPoint_SignatureAggregatorChangedEntityLoaderContext,
      @as("EntryPoint_StakeLocked")
      entryPoint_StakeLocked: entryPoint_StakeLockedEntityLoaderContext,
      @as("EntryPoint_StakeUnlocked")
      entryPoint_StakeUnlocked: entryPoint_StakeUnlockedEntityLoaderContext,
      @as("EntryPoint_StakeWithdrawn")
      entryPoint_StakeWithdrawn: entryPoint_StakeWithdrawnEntityLoaderContext,
      @as("EntryPoint_UserOperationEvent")
      entryPoint_UserOperationEvent: entryPoint_UserOperationEventEntityLoaderContext,
      @as("EntryPoint_UserOperationRevertReason")
      entryPoint_UserOperationRevertReason: entryPoint_UserOperationRevertReasonEntityLoaderContext,
      @as("EntryPoint_Withdrawn") entryPoint_Withdrawn: entryPoint_WithdrawnEntityLoaderContext,
    }
  }
}

type event =
  | EntryPointContract_AccountDeployed(eventLog<EntryPointContract.AccountDeployedEvent.eventArgs>)
  | EntryPointContract_BeforeExecution(eventLog<EntryPointContract.BeforeExecutionEvent.eventArgs>)
  | EntryPointContract_Deposited(eventLog<EntryPointContract.DepositedEvent.eventArgs>)
  | EntryPointContract_SignatureAggregatorChanged(
      eventLog<EntryPointContract.SignatureAggregatorChangedEvent.eventArgs>,
    )
  | EntryPointContract_StakeLocked(eventLog<EntryPointContract.StakeLockedEvent.eventArgs>)
  | EntryPointContract_StakeUnlocked(eventLog<EntryPointContract.StakeUnlockedEvent.eventArgs>)
  | EntryPointContract_StakeWithdrawn(eventLog<EntryPointContract.StakeWithdrawnEvent.eventArgs>)
  | EntryPointContract_UserOperationEvent(
      eventLog<EntryPointContract.UserOperationEventEvent.eventArgs>,
    )
  | EntryPointContract_UserOperationRevertReason(
      eventLog<EntryPointContract.UserOperationRevertReasonEvent.eventArgs>,
    )
  | EntryPointContract_Withdrawn(eventLog<EntryPointContract.WithdrawnEvent.eventArgs>)

type eventName =
  | @as("EntryPoint_AccountDeployed") EntryPoint_AccountDeployed
  | @as("EntryPoint_BeforeExecution") EntryPoint_BeforeExecution
  | @as("EntryPoint_Deposited") EntryPoint_Deposited
  | @as("EntryPoint_SignatureAggregatorChanged") EntryPoint_SignatureAggregatorChanged
  | @as("EntryPoint_StakeLocked") EntryPoint_StakeLocked
  | @as("EntryPoint_StakeUnlocked") EntryPoint_StakeUnlocked
  | @as("EntryPoint_StakeWithdrawn") EntryPoint_StakeWithdrawn
  | @as("EntryPoint_UserOperationEvent") EntryPoint_UserOperationEvent
  | @as("EntryPoint_UserOperationRevertReason") EntryPoint_UserOperationRevertReason
  | @as("EntryPoint_Withdrawn") EntryPoint_Withdrawn
// true
let eventNameSchema = S.union([
  S.literal(EntryPoint_AccountDeployed),
  S.literal(EntryPoint_BeforeExecution),
  S.literal(EntryPoint_Deposited),
  S.literal(EntryPoint_SignatureAggregatorChanged),
  S.literal(EntryPoint_StakeLocked),
  S.literal(EntryPoint_StakeUnlocked),
  S.literal(EntryPoint_StakeWithdrawn),
  S.literal(EntryPoint_UserOperationEvent),
  S.literal(EntryPoint_UserOperationRevertReason),
  S.literal(EntryPoint_Withdrawn),
])

let eventNameToString = (eventName: eventName) =>
  switch eventName {
  | EntryPoint_AccountDeployed => "AccountDeployed"
  | EntryPoint_BeforeExecution => "BeforeExecution"
  | EntryPoint_Deposited => "Deposited"
  | EntryPoint_SignatureAggregatorChanged => "SignatureAggregatorChanged"
  | EntryPoint_StakeLocked => "StakeLocked"
  | EntryPoint_StakeUnlocked => "StakeUnlocked"
  | EntryPoint_StakeWithdrawn => "StakeWithdrawn"
  | EntryPoint_UserOperationEvent => "UserOperationEvent"
  | EntryPoint_UserOperationRevertReason => "UserOperationRevertReason"
  | EntryPoint_Withdrawn => "Withdrawn"
  }

exception UnknownEvent(string, string)
let eventTopicToEventName = (contractName, topic0) =>
  switch (contractName, topic0) {
  | ("EntryPoint", "0xd51a9c61267aa6196961883ecf5ff2da6619c37dac0fa92122513fb32c032d2d") =>
    EntryPoint_AccountDeployed
  | ("EntryPoint", "0xbb47ee3e183a558b1a2ff0874b079f3fc5478b7454eacf2bfc5af2ff5878f972") =>
    EntryPoint_BeforeExecution
  | ("EntryPoint", "0x2da466a7b24304f47e87fa2e1e5a81b9831ce54fec19055ce277ca2f39ba42c4") =>
    EntryPoint_Deposited
  | ("EntryPoint", "0x575ff3acadd5ab348fe1855e217e0f3678f8d767d7494c9f9fefbee2e17cca4d") =>
    EntryPoint_SignatureAggregatorChanged
  | ("EntryPoint", "0xa5ae833d0bb1dcd632d98a8b70973e8516812898e19bf27b70071ebc8dc52c01") =>
    EntryPoint_StakeLocked
  | ("EntryPoint", "0xfa9b3c14cc825c412c9ed81b3ba365a5b459439403f18829e572ed53a4180f0a") =>
    EntryPoint_StakeUnlocked
  | ("EntryPoint", "0xb7c918e0e249f999e965cafeb6c664271b3f4317d296461500e71da39f0cbda3") =>
    EntryPoint_StakeWithdrawn
  | ("EntryPoint", "0x49628fd1471006c1482da88028e9ce4dbb080b815c9b0344d39e5a8e6ec1419f") =>
    EntryPoint_UserOperationEvent
  | ("EntryPoint", "0x1c4fada7374c0a9ee8841fc38afe82932dc0f8e69012e927f061a8bae611a201") =>
    EntryPoint_UserOperationRevertReason
  | ("EntryPoint", "0xd1c19fbcd4551a5edfb66d43d2e337c04837afda3482b42bdf569a8fccdae5fb") =>
    EntryPoint_Withdrawn
  | (contractName, topic0) => UnknownEvent(contractName, topic0)->raise
  }

@genType
type chainId = int

type eventBatchQueueItem = {
  timestamp: int,
  chain: ChainMap.Chain.t,
  blockNumber: int,
  logIndex: int,
  event: event,
  //Default to false, if an event needs to
  //be reprocessed after it has loaded dynamic contracts
  //This gets set to true and does not try and reload events
  hasRegisteredDynamicContracts?: bool,
}
