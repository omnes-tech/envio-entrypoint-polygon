type entityGetters = {
  getEntryPoint_AccountDeployed: Types.id => promise<array<Entities.EntryPoint_AccountDeployed.t>>,
  getEntryPoint_BeforeExecution: Types.id => promise<array<Entities.EntryPoint_BeforeExecution.t>>,
  getEntryPoint_Deposited: Types.id => promise<array<Entities.EntryPoint_Deposited.t>>,
  getEntryPoint_SignatureAggregatorChanged: Types.id => promise<
    array<Entities.EntryPoint_SignatureAggregatorChanged.t>,
  >,
  getEntryPoint_StakeLocked: Types.id => promise<array<Entities.EntryPoint_StakeLocked.t>>,
  getEntryPoint_StakeUnlocked: Types.id => promise<array<Entities.EntryPoint_StakeUnlocked.t>>,
  getEntryPoint_StakeWithdrawn: Types.id => promise<array<Entities.EntryPoint_StakeWithdrawn.t>>,
  getEntryPoint_UserOperationEvent: Types.id => promise<
    array<Entities.EntryPoint_UserOperationEvent.t>,
  >,
  getEntryPoint_UserOperationRevertReason: Types.id => promise<
    array<Entities.EntryPoint_UserOperationRevertReason.t>,
  >,
  getEntryPoint_Withdrawn: Types.id => promise<array<Entities.EntryPoint_Withdrawn.t>>,
}

@genType
type genericContextCreatorFunctions<'loaderContext, 'handlerContextSync, 'handlerContextAsync> = {
  logger: Pino.t,
  log: Logs.userLogger,
  getLoaderContext: unit => 'loaderContext,
  getHandlerContextSync: unit => 'handlerContextSync,
  getHandlerContextAsync: unit => 'handlerContextAsync,
  getEntitiesToLoad: unit => array<Types.entityRead>,
  getAddedDynamicContractRegistrations: unit => array<Types.dynamicContractRegistryEntity>,
}

type contextCreator<'eventArgs, 'loaderContext, 'handlerContext, 'handlerContextAsync> = (
  ~inMemoryStore: IO.InMemoryStore.t,
  ~chainId: int,
  ~event: Types.eventLog<'eventArgs>,
  ~logger: Pino.t,
  ~asyncGetters: entityGetters,
) => genericContextCreatorFunctions<'loaderContext, 'handlerContext, 'handlerContextAsync>

let getEventIdentifier = (event: Types.eventLog<'a>, ~chainId): Types.eventIdentifier => {
  chainId,
  blockTimestamp: event.blockTimestamp,
  blockNumber: event.blockNumber,
  logIndex: event.logIndex,
}

exception UnableToLoadNonNullableLinkedEntity(string)
exception LinkedEntityNotAvailableInSyncHandler(string)

module EntryPointContract = {
  module AccountDeployedEvent = {
    type loaderContext = Types.EntryPointContract.AccountDeployedEvent.loaderContext
    type handlerContext = Types.EntryPointContract.AccountDeployedEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.AccountDeployedEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.AccountDeployedEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.AccountDeployed",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module BeforeExecutionEvent = {
    type loaderContext = Types.EntryPointContract.BeforeExecutionEvent.loaderContext
    type handlerContext = Types.EntryPointContract.BeforeExecutionEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.BeforeExecutionEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.BeforeExecutionEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.BeforeExecution",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module DepositedEvent = {
    type loaderContext = Types.EntryPointContract.DepositedEvent.loaderContext
    type handlerContext = Types.EntryPointContract.DepositedEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.DepositedEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.DepositedEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.Deposited",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module SignatureAggregatorChangedEvent = {
    type loaderContext = Types.EntryPointContract.SignatureAggregatorChangedEvent.loaderContext
    type handlerContext = Types.EntryPointContract.SignatureAggregatorChangedEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.SignatureAggregatorChangedEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.SignatureAggregatorChanged",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module StakeLockedEvent = {
    type loaderContext = Types.EntryPointContract.StakeLockedEvent.loaderContext
    type handlerContext = Types.EntryPointContract.StakeLockedEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.StakeLockedEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.StakeLockedEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.StakeLocked",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module StakeUnlockedEvent = {
    type loaderContext = Types.EntryPointContract.StakeUnlockedEvent.loaderContext
    type handlerContext = Types.EntryPointContract.StakeUnlockedEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.StakeUnlockedEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.StakeUnlockedEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.StakeUnlocked",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module StakeWithdrawnEvent = {
    type loaderContext = Types.EntryPointContract.StakeWithdrawnEvent.loaderContext
    type handlerContext = Types.EntryPointContract.StakeWithdrawnEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.StakeWithdrawnEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.StakeWithdrawnEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.StakeWithdrawn",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module UserOperationEventEvent = {
    type loaderContext = Types.EntryPointContract.UserOperationEventEvent.loaderContext
    type handlerContext = Types.EntryPointContract.UserOperationEventEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.UserOperationEventEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.UserOperationEventEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.UserOperationEvent",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module UserOperationRevertReasonEvent = {
    type loaderContext = Types.EntryPointContract.UserOperationRevertReasonEvent.loaderContext
    type handlerContext = Types.EntryPointContract.UserOperationRevertReasonEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.UserOperationRevertReasonEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.UserOperationRevertReason",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }

  module WithdrawnEvent = {
    type loaderContext = Types.EntryPointContract.WithdrawnEvent.loaderContext
    type handlerContext = Types.EntryPointContract.WithdrawnEvent.handlerContext
    type handlerContextAsync = Types.EntryPointContract.WithdrawnEvent.handlerContextAsync
    type context = genericContextCreatorFunctions<
      loaderContext,
      handlerContext,
      handlerContextAsync,
    >

    let contextCreator: contextCreator<
      Types.EntryPointContract.WithdrawnEvent.eventArgs,
      loaderContext,
      handlerContext,
      handlerContextAsync,
    > = (~inMemoryStore, ~chainId, ~event, ~logger, ~asyncGetters) => {
      let eventIdentifier = event->getEventIdentifier(~chainId)
      // NOTE: we could optimise this code to onle create a logger if there was a log called.
      let logger = logger->Logging.createChildFrom(
        ~logger=_,
        ~params={
          "context": "EntryPoint.Withdrawn",
          "chainId": chainId,
          "block": event.blockNumber,
          "logIndex": event.logIndex,
          "txHash": event.transactionHash,
        },
      )

      let contextLogger: Logs.userLogger = {
        info: (message: string) => logger->Logging.uinfo(message),
        debug: (message: string) => logger->Logging.udebug(message),
        warn: (message: string) => logger->Logging.uwarn(message),
        error: (message: string) => logger->Logging.uerror(message),
        errorWithExn: (exn: option<Js.Exn.t>, message: string) =>
          logger->Logging.uerrorWithExn(exn, message),
      }

      let optSetOfIds_entryPoint_AccountDeployed: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_BeforeExecution: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Deposited: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_SignatureAggregatorChanged: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeLocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeUnlocked: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_StakeWithdrawn: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationEvent: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_UserOperationRevertReason: Set.t<Types.id> = Set.make()
      let optSetOfIds_entryPoint_Withdrawn: Set.t<Types.id> = Set.make()

      let entitiesToLoad: array<Types.entityRead> = []

      let addedDynamicContractRegistrations: array<Types.dynamicContractRegistryEntity> = []

      //Loader context can be defined as a value and the getter can return that value

      @warning("-16")
      let loaderContext: loaderContext = {
        log: contextLogger,
        contractRegistration: {
          //TODO only add contracts we've registered for the event in the config
          addEntryPoint: (contractAddress: Ethers.ethAddress) => {
            let eventId = EventUtils.packEventIndex(
              ~blockNumber=event.blockNumber,
              ~logIndex=event.logIndex,
            )
            let dynamicContractRegistration: Types.dynamicContractRegistryEntity = {
              chainId,
              eventId,
              blockTimestamp: event.blockTimestamp,
              contractAddress,
              contractType: "EntryPoint",
            }

            addedDynamicContractRegistrations->Js.Array2.push(dynamicContractRegistration)->ignore

            inMemoryStore.dynamicContractRegistry->IO.InMemoryStore.DynamicContractRegistry.set(
              ~key={chainId, contractAddress},
              ~entity=dynamicContractRegistration,
            )
          },
        },
        entryPoint_AccountDeployed: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_AccountDeployed->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_AccountDeployedRead(id))
          },
        },
        entryPoint_BeforeExecution: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_BeforeExecution->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_BeforeExecutionRead(id))
          },
        },
        entryPoint_Deposited: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Deposited->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_DepositedRead(id))
          },
        },
        entryPoint_SignatureAggregatorChanged: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_SignatureAggregatorChangedRead(id),
            )
          },
        },
        entryPoint_StakeLocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeLocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeLockedRead(id))
          },
        },
        entryPoint_StakeUnlocked: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeUnlocked->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeUnlockedRead(id))
          },
        },
        entryPoint_StakeWithdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_StakeWithdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_StakeWithdrawnRead(id))
          },
        },
        entryPoint_UserOperationEvent: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationEvent->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_UserOperationEventRead(id))
          },
        },
        entryPoint_UserOperationRevertReason: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_UserOperationRevertReason->Set.add(id)
            let _ = Js.Array2.push(
              entitiesToLoad,
              Types.EntryPoint_UserOperationRevertReasonRead(id),
            )
          },
        },
        entryPoint_Withdrawn: {
          load: (id: Types.id) => {
            let _ = optSetOfIds_entryPoint_Withdrawn->Set.add(id)
            let _ = Js.Array2.push(entitiesToLoad, Types.EntryPoint_WithdrawnRead(id))
          },
        },
      }

      //handler context must be defined as a getter function so that it can construct the context
      //without stale values whenever it is used
      let getHandlerContextSync: unit => handlerContext = () => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_AccountDeployed" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_AccountDeployed.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_BeforeExecution" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_BeforeExecution.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Deposited" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Deposited.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_SignatureAggregatorChanged" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_SignatureAggregatorChanged.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeLocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeLocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeUnlocked" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeUnlocked.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_StakeWithdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_StakeWithdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationEvent" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationEvent.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_UserOperationRevertReason" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_UserOperationRevertReason.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                Logging.warn(
                  `The loader for a "EntryPoint_Withdrawn" of entity with id "${id}" was not used please add it to your default loader function (ie. place 'context.entryPoint_Withdrawn.load("${id}")' inside your loader) to avoid unexpected behaviour. This is a runtime validation check.`,
                )

                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)

                // TODO: add a further step to synchronously try fetch this from the DB if it isn't in the in-memory store - similar to this PR: https://github.com/Float-Capital/indexer/pull/759
              }
            },
          },
        }
      }

      let getHandlerContextAsync = (): handlerContextAsync => {
        {
          log: contextLogger,
          entryPoint_AccountDeployed: {
            set: entity => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_AccountDeployed->Set.has(id) {
                inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_AccountDeployed->IO.InMemoryStore.EntryPoint_AccountDeployed.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_AccountDeployed(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_AccountDeployed.initValue(
                    inMemoryStore.entryPoint_AccountDeployed,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_BeforeExecution: {
            set: entity => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_BeforeExecution->Set.has(id) {
                inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_BeforeExecution->IO.InMemoryStore.EntryPoint_BeforeExecution.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_BeforeExecution(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_BeforeExecution.initValue(
                    inMemoryStore.entryPoint_BeforeExecution,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Deposited: {
            set: entity => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Deposited->Set.has(id) {
                inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Deposited->IO.InMemoryStore.EntryPoint_Deposited.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Deposited(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Deposited.initValue(
                    inMemoryStore.entryPoint_Deposited,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_SignatureAggregatorChanged: {
            set: entity => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_SignatureAggregatorChanged->Set.has(id) {
                inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_SignatureAggregatorChanged->IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_SignatureAggregatorChanged(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue(
                    inMemoryStore.entryPoint_SignatureAggregatorChanged,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeLocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeLocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeLocked->IO.InMemoryStore.EntryPoint_StakeLocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeLocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeLocked.initValue(
                    inMemoryStore.entryPoint_StakeLocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeUnlocked: {
            set: entity => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeUnlocked->Set.has(id) {
                inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeUnlocked->IO.InMemoryStore.EntryPoint_StakeUnlocked.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeUnlocked(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeUnlocked.initValue(
                    inMemoryStore.entryPoint_StakeUnlocked,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_StakeWithdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_StakeWithdrawn->Set.has(id) {
                inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_StakeWithdrawn->IO.InMemoryStore.EntryPoint_StakeWithdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_StakeWithdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_StakeWithdrawn.initValue(
                    inMemoryStore.entryPoint_StakeWithdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationEvent: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationEvent->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationEvent->IO.InMemoryStore.EntryPoint_UserOperationEvent.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationEvent(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationEvent.initValue(
                    inMemoryStore.entryPoint_UserOperationEvent,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_UserOperationRevertReason: {
            set: entity => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_UserOperationRevertReason->Set.has(id) {
                inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                )
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_UserOperationRevertReason->IO.InMemoryStore.EntryPoint_UserOperationRevertReason.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_UserOperationRevertReason(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_UserOperationRevertReason.initValue(
                    inMemoryStore.entryPoint_UserOperationRevertReason,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
          entryPoint_Withdrawn: {
            set: entity => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=entity.id,
                ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            deleteUnsafe: id => {
              inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.set(
                ~key=id,
                ~entity=Delete(id)->Types.mkEntityUpdate(~eventIdentifier),
              )
            },
            get: async (id: Types.id) => {
              if optSetOfIds_entryPoint_Withdrawn->Set.has(id) {
                inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(id)
              } else {
                // NOTE: this will still return the value if it exists in the in-memory store (despite the loader not being run).
                switch inMemoryStore.entryPoint_Withdrawn->IO.InMemoryStore.EntryPoint_Withdrawn.get(
                  id,
                ) {
                | Some(entity) => Some(entity)
                | None =>
                  let entities = await asyncGetters.getEntryPoint_Withdrawn(id)

                  let optEntity = entities->Belt.Array.get(0)

                  IO.InMemoryStore.EntryPoint_Withdrawn.initValue(
                    inMemoryStore.entryPoint_Withdrawn,
                    ~key=id,
                    ~entity=optEntity,
                  )

                  optEntity
                }
              }
            },
          },
        }
      }

      {
        logger,
        log: contextLogger,
        getEntitiesToLoad: () => entitiesToLoad,
        getAddedDynamicContractRegistrations: () => addedDynamicContractRegistrations,
        getLoaderContext: () => loaderContext,
        getHandlerContextSync,
        getHandlerContextAsync,
      }
    }
  }
}

type eventAndContext =
  | EntryPointContract_AccountDeployedWithContext(
      Types.eventLog<Types.EntryPointContract.AccountDeployedEvent.eventArgs>,
      EntryPointContract.AccountDeployedEvent.context,
    )
  | EntryPointContract_BeforeExecutionWithContext(
      Types.eventLog<Types.EntryPointContract.BeforeExecutionEvent.eventArgs>,
      EntryPointContract.BeforeExecutionEvent.context,
    )
  | EntryPointContract_DepositedWithContext(
      Types.eventLog<Types.EntryPointContract.DepositedEvent.eventArgs>,
      EntryPointContract.DepositedEvent.context,
    )
  | EntryPointContract_SignatureAggregatorChangedWithContext(
      Types.eventLog<Types.EntryPointContract.SignatureAggregatorChangedEvent.eventArgs>,
      EntryPointContract.SignatureAggregatorChangedEvent.context,
    )
  | EntryPointContract_StakeLockedWithContext(
      Types.eventLog<Types.EntryPointContract.StakeLockedEvent.eventArgs>,
      EntryPointContract.StakeLockedEvent.context,
    )
  | EntryPointContract_StakeUnlockedWithContext(
      Types.eventLog<Types.EntryPointContract.StakeUnlockedEvent.eventArgs>,
      EntryPointContract.StakeUnlockedEvent.context,
    )
  | EntryPointContract_StakeWithdrawnWithContext(
      Types.eventLog<Types.EntryPointContract.StakeWithdrawnEvent.eventArgs>,
      EntryPointContract.StakeWithdrawnEvent.context,
    )
  | EntryPointContract_UserOperationEventWithContext(
      Types.eventLog<Types.EntryPointContract.UserOperationEventEvent.eventArgs>,
      EntryPointContract.UserOperationEventEvent.context,
    )
  | EntryPointContract_UserOperationRevertReasonWithContext(
      Types.eventLog<Types.EntryPointContract.UserOperationRevertReasonEvent.eventArgs>,
      EntryPointContract.UserOperationRevertReasonEvent.context,
    )
  | EntryPointContract_WithdrawnWithContext(
      Types.eventLog<Types.EntryPointContract.WithdrawnEvent.eventArgs>,
      EntryPointContract.WithdrawnEvent.context,
    )

type eventRouterEventAndContext = {
  chainId: int,
  event: eventAndContext,
}
