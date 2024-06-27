module InMemoryStore = {
  type stringHasher<'val> = 'val => string

  type storeStateEntity<'entity, 'entityKey> = {
    dict: Js.Dict.t<Types.inMemoryStoreRowEntity<'entity>>,
    hasher: stringHasher<'entityKey>,
  }

  type storeStateMeta<'entity, 'entityKey> = {
    dict: Js.Dict.t<Types.inMemoryStoreRowMeta<'entity>>,
    hasher: stringHasher<'entityKey>,
  }

  module type StoreItem = {
    type t
    type key
    let hasher: stringHasher<key>
  }

  //Binding used for deep cloning stores in tests
  @val external structuredClone: 'a => 'a = "structuredClone"

  module MakeStoreEntity = (StoreItem: StoreItem) => {
    @genType
    type value = StoreItem.t
    @genType
    type key = StoreItem.key
    type t = storeStateEntity<value, key>

    let make = (): t => {dict: Js.Dict.empty(), hasher: StoreItem.hasher}

    let initValue = (
      // NOTE: This value is only set to true in the internals of the test framework to create the mockDb.
      ~allowOverWriteEntity=false,
      ~key: StoreItem.key,
      ~entity: option<StoreItem.t>,
      self: t,
    ) => {
      let shouldWriteEntity =
        allowOverWriteEntity || self.dict->Js.Dict.get(key->self.hasher)->Belt.Option.isNone

      //Only initialize a row in the case where it is none
      //or if allowOverWriteEntity is true (used for mockDb in test helpers)
      if shouldWriteEntity {
        let initialStoreRow: Types.inMemoryStoreRowEntity<StoreItem.t> = switch entity {
        | Some(entity) => InitialReadFromDb(AlreadySet(entity))
        | None => InitialReadFromDb(NotSet)
        }
        self.dict->Js.Dict.set(key->self.hasher, initialStoreRow)
      }
    }

    let set = (self: t, ~key: StoreItem.key, ~entity: Types.entityUpdate<StoreItem.t>) => {
      let mapKey = key->self.hasher
      let currentEntity = self.dict->Js.Dict.get(mapKey)
      let entityData: Types.inMemoryStoreRowEntity<StoreItem.t> = switch currentEntity {
      | Some(InitialReadFromDb(entity_read)) =>
        Updated({
          initial: Retrieved(entity_read),
          latest: entity,
          history: [],
        })
      | Some(Updated(previous_values))
        if !Config.shouldRollbackOnReorg ||
        //Rollback initial state cases should not save history
        !previous_values.latest.shouldSaveHistory ||
        // This prevents two db actions in the same event on the same entity from being recorded to the history table.
        previous_values.latest.eventIdentifier == entity.eventIdentifier =>
        Updated({
          ...previous_values,
          latest: entity,
        })
      | Some(Updated(previous_values)) =>
        Updated({
          initial: previous_values.initial,
          latest: entity,
          history: previous_values.history->Belt.Array.concat([previous_values.latest]),
        })
      | None =>
        Updated({
          initial: Unknown,
          latest: entity,
          history: [],
        })
      }
      self.dict->Js.Dict.set(mapKey, entityData)
    }

    let get = (self: t, key: StoreItem.key) =>
      self.dict
      ->Js.Dict.get(key->self.hasher)
      ->Belt.Option.flatMap(row => {
        switch row {
        | Updated({latest: {entityUpdateAction: Set(entity)}}) => Some(entity)
        | Updated({latest: {entityUpdateAction: Delete(_)}}) => None
        | InitialReadFromDb(AlreadySet(entity)) => Some(entity)
        | InitialReadFromDb(NotSet) => None
        }
      })

    let values = (self: t) => self.dict->Js.Dict.values

    let clone = (self: t) => {
      ...self,
      dict: self.dict->structuredClone,
    }
  }

  module MakeStoreMeta = (StoreItem: StoreItem) => {
    @genType
    type value = StoreItem.t
    @genType
    type key = StoreItem.key
    type t = storeStateMeta<value, key>

    let make = (): t => {dict: Js.Dict.empty(), hasher: StoreItem.hasher}

    let set = (self: t, ~key: StoreItem.key, ~entity: StoreItem.t) =>
      self.dict->Js.Dict.set(key->self.hasher, entity)

    let get = (self: t, key: StoreItem.key) =>
      self.dict->Js.Dict.get(key->self.hasher)->Belt.Option.map(row => row)

    let values = (self: t) => self.dict->Js.Dict.values

    let clone = (self: t) => {
      ...self,
      dict: self.dict->structuredClone,
    }
  }

  module EventSyncState = MakeStoreMeta({
    type t = DbFunctions.EventSyncState.eventSyncState
    type key = int
    let hasher = Belt.Int.toString
  })

  @genType
  type rawEventsKey = {
    chainId: int,
    eventId: string,
  }

  module RawEvents = MakeStoreMeta({
    type t = Types.rawEventsEntity
    type key = rawEventsKey
    let hasher = (key: key) =>
      EventUtils.getEventIdKeyString(~chainId=key.chainId, ~eventId=key.eventId)
  })

  @genType
  type dynamicContractRegistryKey = {
    chainId: int,
    contractAddress: Ethers.ethAddress,
  }

  module DynamicContractRegistry = MakeStoreMeta({
    type t = Types.dynamicContractRegistryEntity
    type key = dynamicContractRegistryKey
    let hasher = ({chainId, contractAddress}) =>
      EventUtils.getContractAddressKeyString(~chainId, ~contractAddress)
  })

  module EntryPoint_AccountDeployed = MakeStoreEntity({
    type t = Entities.EntryPoint_AccountDeployed.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_BeforeExecution = MakeStoreEntity({
    type t = Entities.EntryPoint_BeforeExecution.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_Deposited = MakeStoreEntity({
    type t = Entities.EntryPoint_Deposited.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_SignatureAggregatorChanged = MakeStoreEntity({
    type t = Entities.EntryPoint_SignatureAggregatorChanged.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_StakeLocked = MakeStoreEntity({
    type t = Entities.EntryPoint_StakeLocked.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_StakeUnlocked = MakeStoreEntity({
    type t = Entities.EntryPoint_StakeUnlocked.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_StakeWithdrawn = MakeStoreEntity({
    type t = Entities.EntryPoint_StakeWithdrawn.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_UserOperationEvent = MakeStoreEntity({
    type t = Entities.EntryPoint_UserOperationEvent.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_UserOperationRevertReason = MakeStoreEntity({
    type t = Entities.EntryPoint_UserOperationRevertReason.t
    type key = string
    let hasher = Obj.magic
  })

  module EntryPoint_Withdrawn = MakeStoreEntity({
    type t = Entities.EntryPoint_Withdrawn.t
    type key = string
    let hasher = Obj.magic
  })

  @genType
  type t = {
    eventSyncState: EventSyncState.t,
    rawEvents: RawEvents.t,
    dynamicContractRegistry: DynamicContractRegistry.t,
    entryPoint_AccountDeployed: EntryPoint_AccountDeployed.t,
    entryPoint_BeforeExecution: EntryPoint_BeforeExecution.t,
    entryPoint_Deposited: EntryPoint_Deposited.t,
    entryPoint_SignatureAggregatorChanged: EntryPoint_SignatureAggregatorChanged.t,
    entryPoint_StakeLocked: EntryPoint_StakeLocked.t,
    entryPoint_StakeUnlocked: EntryPoint_StakeUnlocked.t,
    entryPoint_StakeWithdrawn: EntryPoint_StakeWithdrawn.t,
    entryPoint_UserOperationEvent: EntryPoint_UserOperationEvent.t,
    entryPoint_UserOperationRevertReason: EntryPoint_UserOperationRevertReason.t,
    entryPoint_Withdrawn: EntryPoint_Withdrawn.t,
    rollBackEventIdentifier: option<Types.eventIdentifier>,
  }

  let makeWithRollBackEventIdentifier = (rollBackEventIdentifier): t => {
    eventSyncState: EventSyncState.make(),
    rawEvents: RawEvents.make(),
    dynamicContractRegistry: DynamicContractRegistry.make(),
    entryPoint_AccountDeployed: EntryPoint_AccountDeployed.make(),
    entryPoint_BeforeExecution: EntryPoint_BeforeExecution.make(),
    entryPoint_Deposited: EntryPoint_Deposited.make(),
    entryPoint_SignatureAggregatorChanged: EntryPoint_SignatureAggregatorChanged.make(),
    entryPoint_StakeLocked: EntryPoint_StakeLocked.make(),
    entryPoint_StakeUnlocked: EntryPoint_StakeUnlocked.make(),
    entryPoint_StakeWithdrawn: EntryPoint_StakeWithdrawn.make(),
    entryPoint_UserOperationEvent: EntryPoint_UserOperationEvent.make(),
    entryPoint_UserOperationRevertReason: EntryPoint_UserOperationRevertReason.make(),
    entryPoint_Withdrawn: EntryPoint_Withdrawn.make(),
    rollBackEventIdentifier,
  }

  let make = () => makeWithRollBackEventIdentifier(None)

  let clone = (self: t) => {
    eventSyncState: self.eventSyncState->EventSyncState.clone,
    rawEvents: self.rawEvents->RawEvents.clone,
    dynamicContractRegistry: self.dynamicContractRegistry->DynamicContractRegistry.clone,
    entryPoint_AccountDeployed: self.entryPoint_AccountDeployed->EntryPoint_AccountDeployed.clone,
    entryPoint_BeforeExecution: self.entryPoint_BeforeExecution->EntryPoint_BeforeExecution.clone,
    entryPoint_Deposited: self.entryPoint_Deposited->EntryPoint_Deposited.clone,
    entryPoint_SignatureAggregatorChanged: self.entryPoint_SignatureAggregatorChanged->EntryPoint_SignatureAggregatorChanged.clone,
    entryPoint_StakeLocked: self.entryPoint_StakeLocked->EntryPoint_StakeLocked.clone,
    entryPoint_StakeUnlocked: self.entryPoint_StakeUnlocked->EntryPoint_StakeUnlocked.clone,
    entryPoint_StakeWithdrawn: self.entryPoint_StakeWithdrawn->EntryPoint_StakeWithdrawn.clone,
    entryPoint_UserOperationEvent: self.entryPoint_UserOperationEvent->EntryPoint_UserOperationEvent.clone,
    entryPoint_UserOperationRevertReason: self.entryPoint_UserOperationRevertReason->EntryPoint_UserOperationRevertReason.clone,
    entryPoint_Withdrawn: self.entryPoint_Withdrawn->EntryPoint_Withdrawn.clone,
    rollBackEventIdentifier: self.rollBackEventIdentifier->structuredClone,
  }
}

module LoadLayer = {
  /**The ids to load for a particular entity*/
  type idsToLoad = Belt.Set.String.t

  /**
  A round of entities to load from the DB. Depending on what entities come back
  and the dataLoaded "actions" that get run after the entities are loaded up. It
  could mean another load layer is created based of values that are returned
  */
  type rec t = {
    //A an array of getters to run after the entities with idsToLoad have been loaded
    dataLoadedActionsGetters: dataLoadedActionsGetters,
    //A unique list of ids that need to be loaded for entity entryPoint_AccountDeployed
    entryPoint_AccountDeployedIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_BeforeExecution
    entryPoint_BeforeExecutionIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_Deposited
    entryPoint_DepositedIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_SignatureAggregatorChanged
    entryPoint_SignatureAggregatorChangedIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_StakeLocked
    entryPoint_StakeLockedIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_StakeUnlocked
    entryPoint_StakeUnlockedIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_StakeWithdrawn
    entryPoint_StakeWithdrawnIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_UserOperationEvent
    entryPoint_UserOperationEventIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_UserOperationRevertReason
    entryPoint_UserOperationRevertReasonIdsToLoad: idsToLoad,
    //A unique list of ids that need to be loaded for entity entryPoint_Withdrawn
    entryPoint_WithdrawnIdsToLoad: idsToLoad,
  }
  //An action that gets run after the data is loaded in from the db to the in memory store
  //the action will derive values from the loaded data and update the next load layer
  and dataLoadedAction = t => t
  //A getter function that returns an array of actions that need to be run
  //Actions will fetch values from the in memory store and update a load layer
  and dataLoadedActionsGetter = unit => array<dataLoadedAction>
  //An array of getter functions for dataLoadedActions
  and dataLoadedActionsGetters = array<dataLoadedActionsGetter>

  /**Instantiates a load layer*/
  let emptyLoadLayer = () => {
    entryPoint_AccountDeployedIdsToLoad: Belt.Set.String.empty,
    entryPoint_BeforeExecutionIdsToLoad: Belt.Set.String.empty,
    entryPoint_DepositedIdsToLoad: Belt.Set.String.empty,
    entryPoint_SignatureAggregatorChangedIdsToLoad: Belt.Set.String.empty,
    entryPoint_StakeLockedIdsToLoad: Belt.Set.String.empty,
    entryPoint_StakeUnlockedIdsToLoad: Belt.Set.String.empty,
    entryPoint_StakeWithdrawnIdsToLoad: Belt.Set.String.empty,
    entryPoint_UserOperationEventIdsToLoad: Belt.Set.String.empty,
    entryPoint_UserOperationRevertReasonIdsToLoad: Belt.Set.String.empty,
    entryPoint_WithdrawnIdsToLoad: Belt.Set.String.empty,
    dataLoadedActionsGetters: [],
  }

  /* Helper to append an ID to load for a given entity to the loadLayer */
  let extendIdsToLoad = (idsToLoad: idsToLoad, entityId: Types.id): idsToLoad =>
    idsToLoad->Belt.Set.String.add(entityId)

  /* Helper to append a getter for DataLoadedActions to load for a given entity to the loadLayer */
  let extendDataLoadedActionsGetters = (
    dataLoadedActionsGetters: dataLoadedActionsGetters,
    newDataLoadedActionsGetters: dataLoadedActionsGetters,
  ): dataLoadedActionsGetters =>
    dataLoadedActionsGetters->Belt.Array.concat(newDataLoadedActionsGetters)
}

//remove warning 39 for unused "rec" flag in case of no other related loaders
/**
Loader functions for each entity. The loader function extends a load layer with the given id and config.
*/
@warning("-39")
let rec entryPoint_AccountDeployedLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_AccountDeployedLoaderConfig: Types.entryPoint_AccountDeployedLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_AccountDeployedLoaderConfig" type is a boolean.
  if !entryPoint_AccountDeployedLoaderConfig {
    //If entryPoint_AccountDeployedLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_AccountDeployedLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_AccountDeployedIdsToLoad: loadLayer.entryPoint_AccountDeployedIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_BeforeExecutionLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_BeforeExecutionLoaderConfig: Types.entryPoint_BeforeExecutionLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_BeforeExecutionLoaderConfig" type is a boolean.
  if !entryPoint_BeforeExecutionLoaderConfig {
    //If entryPoint_BeforeExecutionLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_BeforeExecutionLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_BeforeExecutionIdsToLoad: loadLayer.entryPoint_BeforeExecutionIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_DepositedLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_DepositedLoaderConfig: Types.entryPoint_DepositedLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_DepositedLoaderConfig" type is a boolean.
  if !entryPoint_DepositedLoaderConfig {
    //If entryPoint_DepositedLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_DepositedLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_DepositedIdsToLoad: loadLayer.entryPoint_DepositedIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_SignatureAggregatorChangedLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_SignatureAggregatorChangedLoaderConfig: Types.entryPoint_SignatureAggregatorChangedLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_SignatureAggregatorChangedLoaderConfig" type is a boolean.
  if !entryPoint_SignatureAggregatorChangedLoaderConfig {
    //If entryPoint_SignatureAggregatorChangedLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_SignatureAggregatorChangedLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_SignatureAggregatorChangedIdsToLoad: loadLayer.entryPoint_SignatureAggregatorChangedIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_StakeLockedLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_StakeLockedLoaderConfig: Types.entryPoint_StakeLockedLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_StakeLockedLoaderConfig" type is a boolean.
  if !entryPoint_StakeLockedLoaderConfig {
    //If entryPoint_StakeLockedLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_StakeLockedLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_StakeLockedIdsToLoad: loadLayer.entryPoint_StakeLockedIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_StakeUnlockedLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_StakeUnlockedLoaderConfig: Types.entryPoint_StakeUnlockedLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_StakeUnlockedLoaderConfig" type is a boolean.
  if !entryPoint_StakeUnlockedLoaderConfig {
    //If entryPoint_StakeUnlockedLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_StakeUnlockedLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_StakeUnlockedIdsToLoad: loadLayer.entryPoint_StakeUnlockedIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_StakeWithdrawnLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_StakeWithdrawnLoaderConfig: Types.entryPoint_StakeWithdrawnLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_StakeWithdrawnLoaderConfig" type is a boolean.
  if !entryPoint_StakeWithdrawnLoaderConfig {
    //If entryPoint_StakeWithdrawnLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_StakeWithdrawnLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_StakeWithdrawnIdsToLoad: loadLayer.entryPoint_StakeWithdrawnIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_UserOperationEventLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_UserOperationEventLoaderConfig: Types.entryPoint_UserOperationEventLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_UserOperationEventLoaderConfig" type is a boolean.
  if !entryPoint_UserOperationEventLoaderConfig {
    //If entryPoint_UserOperationEventLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_UserOperationEventLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_UserOperationEventIdsToLoad: loadLayer.entryPoint_UserOperationEventIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_UserOperationRevertReasonLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_UserOperationRevertReasonLoaderConfig: Types.entryPoint_UserOperationRevertReasonLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_UserOperationRevertReasonLoaderConfig" type is a boolean.
  if !entryPoint_UserOperationRevertReasonLoaderConfig {
    //If entryPoint_UserOperationRevertReasonLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_UserOperationRevertReasonLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_UserOperationRevertReasonIdsToLoad: loadLayer.entryPoint_UserOperationRevertReasonIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}
@warning("-27")
and entryPoint_WithdrawnLinkedEntityLoader = (
  loadLayer: LoadLayer.t,
  ~entityId: string,
  ~inMemoryStore: InMemoryStore.t,
  ~entryPoint_WithdrawnLoaderConfig: Types.entryPoint_WithdrawnLoaderConfig,
): LoadLayer.t => {
  //No dataLoaded actions need to happen on the in memory
  //since there are no relational non-derivedfrom params
  let _ = inMemoryStore //ignore inMemoryStore and stop warning

  //In this case the "entryPoint_WithdrawnLoaderConfig" type is a boolean.
  if !entryPoint_WithdrawnLoaderConfig {
    //If entryPoint_WithdrawnLoaderConfig is false, don't load the entity
    //simply return the current load layer
    loadLayer
  } else {
    //If entryPoint_WithdrawnLoaderConfig is true,
    //extend the entity ids to load field
    //There can be no dataLoadedActionsGetters to add since this type does not contain
    //any non derived from relational params
    {
      ...loadLayer,
      entryPoint_WithdrawnIdsToLoad: loadLayer.entryPoint_WithdrawnIdsToLoad->LoadLayer.extendIdsToLoad(
        entityId,
      ),
    }
  }
}

/**
Creates and populates a load layer with the current in memory store and an array of entityRead variants
*/
let getLoadLayer = (~entityBatch: array<Types.entityRead>, ~inMemoryStore) => {
  entityBatch->Belt.Array.reduce(LoadLayer.emptyLoadLayer(), (loadLayer, readEntity) => {
    switch readEntity {
    | EntryPoint_AccountDeployedRead(entityId) =>
      loadLayer->entryPoint_AccountDeployedLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_AccountDeployedLoaderConfig=true,
      )
    | EntryPoint_BeforeExecutionRead(entityId) =>
      loadLayer->entryPoint_BeforeExecutionLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_BeforeExecutionLoaderConfig=true,
      )
    | EntryPoint_DepositedRead(entityId) =>
      loadLayer->entryPoint_DepositedLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_DepositedLoaderConfig=true,
      )
    | EntryPoint_SignatureAggregatorChangedRead(entityId) =>
      loadLayer->entryPoint_SignatureAggregatorChangedLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_SignatureAggregatorChangedLoaderConfig=true,
      )
    | EntryPoint_StakeLockedRead(entityId) =>
      loadLayer->entryPoint_StakeLockedLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_StakeLockedLoaderConfig=true,
      )
    | EntryPoint_StakeUnlockedRead(entityId) =>
      loadLayer->entryPoint_StakeUnlockedLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_StakeUnlockedLoaderConfig=true,
      )
    | EntryPoint_StakeWithdrawnRead(entityId) =>
      loadLayer->entryPoint_StakeWithdrawnLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_StakeWithdrawnLoaderConfig=true,
      )
    | EntryPoint_UserOperationEventRead(entityId) =>
      loadLayer->entryPoint_UserOperationEventLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_UserOperationEventLoaderConfig=true,
      )
    | EntryPoint_UserOperationRevertReasonRead(entityId) =>
      loadLayer->entryPoint_UserOperationRevertReasonLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_UserOperationRevertReasonLoaderConfig=true,
      )
    | EntryPoint_WithdrawnRead(entityId) =>
      loadLayer->entryPoint_WithdrawnLinkedEntityLoader(
        ~entityId,
        ~inMemoryStore,
        ~entryPoint_WithdrawnLoaderConfig=true,
      )
    }
  })
}

/**
Represents whether a deeper layer needs to be executed or whether the last layer
has been executed
*/
type nextLayer = NextLayer(LoadLayer.t) | LastLayer

let getNextLayer = (~loadLayer: LoadLayer.t) =>
  switch loadLayer.dataLoadedActionsGetters {
  | [] => LastLayer
  | dataLoadedActionsGetters =>
    dataLoadedActionsGetters
    ->Belt.Array.reduce(LoadLayer.emptyLoadLayer(), (loadLayer, getLoadedActions) => {
      //call getLoadedActions returns array of of actions to run against the load layer
      getLoadedActions()->Belt.Array.reduce(loadLayer, (loadLayer, action) => {
        action(loadLayer)
      })
    })
    ->NextLayer
  }

/**
Used for composing a loadlayer executor
*/
type entityExecutor<'executorRes> = {
  idsToLoad: LoadLayer.idsToLoad,
  executor: LoadLayer.idsToLoad => 'executorRes,
}

/**
Compose an execute load layer function. Used to compose an executor
for a postgres db or a mock db in the testing framework.
*/
let executeLoadLayerComposer = (
  ~entityExecutors: array<entityExecutor<'exectuorRes>>,
  ~handleResponses: array<'exectuorRes> => 'nextLoadlayer,
) => {
  entityExecutors
  ->Belt.Array.map(({idsToLoad, executor}) => {
    idsToLoad->executor
  })
  ->handleResponses
}

/**Recursively load layers with execute fn composer. Can be used with async or sync functions*/
let rec executeNestedLoadLayersComposer = (
  ~loadLayer,
  ~inMemoryStore,
  //Could be an execution function that is async or sync
  ~executeLoadLayerFn,
  //A call back function, for async or sync
  ~then,
  //Unit value, either wrapped in a promise or not
  ~unit,
) => {
  executeLoadLayerFn(~loadLayer, ~inMemoryStore)->then(res =>
    switch res {
    | LastLayer => unit
    | NextLayer(loadLayer) =>
      executeNestedLoadLayersComposer(~loadLayer, ~inMemoryStore, ~executeLoadLayerFn, ~then, ~unit)
    }
  )
}

/**Load all entities in the entity batch from the db to the inMemoryStore */
let loadEntitiesToInMemStoreComposer = (
  ~entityBatch,
  ~inMemoryStore,
  ~executeLoadLayerFn,
  ~then,
  ~unit,
) => {
  executeNestedLoadLayersComposer(
    ~inMemoryStore,
    ~loadLayer=getLoadLayer(~inMemoryStore, ~entityBatch),
    ~executeLoadLayerFn,
    ~then,
    ~unit,
  )
}

let makeEntityExecuterComposer = (
  ~idsToLoad: LoadLayer.idsToLoad,
  ~dbReadFn: array<Belt.Set.String.value> => 'a,
  ~inMemStoreInitFn: (~allowOverWriteEntity: bool=?, ~key: 'c, ~entity: option<'d>, 'b) => unit,
  ~store: 'b,
  ~getEntiyId: 'd => 'c,
  ~unit: 'e,
  ~then: ('a, Belt.Array.t<'d> => unit) => 'e,
) => {
  idsToLoad,
  executor: idsToLoad => {
    switch idsToLoad->Belt.Set.String.toArray {
    | [] => unit //Check if there are values so we don't create an unnecessary empty query
    | idsToLoadArray =>
      idsToLoadArray
      ->dbReadFn
      ->then(entities => {
        entities->Belt.Array.forEach(entity => {
          store->inMemStoreInitFn(~key=entity->getEntiyId, ~entity=Some(entity))
        })
        if Config.shouldRollbackOnReorg {
          let setOfIdsNotSavedToDb =
            idsToLoad->Belt.Set.String.removeMany(entities->Belt.Array.map(getEntiyId))
          setOfIdsNotSavedToDb
          ->Belt.Set.String.toArray
          ->Belt.Array.forEach(entityId => {
            store->inMemStoreInitFn(~key=entityId, ~entity=None)
          })
        }
      })
    }
  },
}

/**
Specifically create an sql executor with async functionality
*/
let makeSqlEntityExecuter = (
  type entity,
  ~entityMod: module(Entities.Entity with type t = entity),
  ~idsToLoad: LoadLayer.idsToLoad,
  ~inMemStoreInitFn: (
    ~allowOverWriteEntity: bool=?,
    ~key: string,
    ~entity: option<entity>,
    'store,
  ) => unit,
  ~store: 'store,
  ~getEntiyId: entity => string,
) => {
  makeEntityExecuterComposer(
    ~dbReadFn=Entities.batchRead(~entityMod, DbFunctions.sql),
    ~idsToLoad,
    ~getEntiyId,
    ~store,
    ~inMemStoreInitFn,
    ~then=Promise.thenResolve,
    ~unit=Promise.resolve(),
  )
}

/**
Executes a single load layer using the async sql functions
*/
let executeSqlLoadLayer = (~loadLayer: LoadLayer.t, ~inMemoryStore: InMemoryStore.t) => {
  let entityExecutors = [
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_AccountDeployedIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_AccountDeployed),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_AccountDeployed.initValue,
      ~store=inMemoryStore.entryPoint_AccountDeployed,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_BeforeExecutionIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_BeforeExecution),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_BeforeExecution.initValue,
      ~store=inMemoryStore.entryPoint_BeforeExecution,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_DepositedIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_Deposited),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_Deposited.initValue,
      ~store=inMemoryStore.entryPoint_Deposited,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_SignatureAggregatorChangedIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_SignatureAggregatorChanged),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_SignatureAggregatorChanged.initValue,
      ~store=inMemoryStore.entryPoint_SignatureAggregatorChanged,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_StakeLockedIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_StakeLocked),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_StakeLocked.initValue,
      ~store=inMemoryStore.entryPoint_StakeLocked,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_StakeUnlockedIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_StakeUnlocked),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_StakeUnlocked.initValue,
      ~store=inMemoryStore.entryPoint_StakeUnlocked,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_StakeWithdrawnIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_StakeWithdrawn),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_StakeWithdrawn.initValue,
      ~store=inMemoryStore.entryPoint_StakeWithdrawn,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_UserOperationEventIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_UserOperationEvent),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_UserOperationEvent.initValue,
      ~store=inMemoryStore.entryPoint_UserOperationEvent,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_UserOperationRevertReasonIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_UserOperationRevertReason),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_UserOperationRevertReason.initValue,
      ~store=inMemoryStore.entryPoint_UserOperationRevertReason,
      ~getEntiyId=entity => entity.id,
    ),
    makeSqlEntityExecuter(
      ~idsToLoad=loadLayer.entryPoint_WithdrawnIdsToLoad,
      ~entityMod=module(Entities.EntryPoint_Withdrawn),
      ~inMemStoreInitFn=InMemoryStore.EntryPoint_Withdrawn.initValue,
      ~store=inMemoryStore.entryPoint_Withdrawn,
      ~getEntiyId=entity => entity.id,
    ),
  ]
  let handleResponses = responses => {
    responses
    ->Promise.all
    ->Promise.thenResolve(_ => {
      getNextLayer(~loadLayer)
    })
  }

  executeLoadLayerComposer(~entityExecutors, ~handleResponses)
}

/**Execute loading of entities using sql*/
let loadEntitiesToInMemStore = (~entityBatch, ~inMemoryStore) => {
  loadEntitiesToInMemStoreComposer(
    ~inMemoryStore,
    ~entityBatch,
    ~executeLoadLayerFn=executeSqlLoadLayer,
    ~then=Promise.then,
    ~unit=Promise.resolve(),
  )
}

let executeSet = (
  sql: Postgres.sql,
  ~items: array<'a>,
  ~dbFunction: (Postgres.sql, array<'a>) => promise<unit>,
) => {
  if items->Array.length > 0 {
    sql->dbFunction(items)
  } else {
    Promise.resolve()
  }
}

let getEntityHistoryItems = (entityUpdates, ~entitySchema, ~entityType) => {
  let (_, entityHistoryItems) = entityUpdates->Belt.Array.reduce((None, []), (
    prev: (option<Types.eventIdentifier>, array<DbFunctions.entityHistoryItem>),
    entity: Types.entityUpdate<'a>,
  ) => {
    let (optPreviousEventIdentifier, entityHistoryItems) = prev

    let {eventIdentifier, shouldSaveHistory, entityUpdateAction} = entity
    let entityHistoryItems = if shouldSaveHistory {
      let mapPrev = Belt.Option.map(optPreviousEventIdentifier)
      let (entity_id, params) = switch entityUpdateAction {
      | Set(entity) => (
          (entity->Obj.magic)["id"],
          Some(entity->S.serializeOrRaiseWith(entitySchema)),
        )
      | Delete(event_id) => (event_id, None)
      }
      let historyItem: DbFunctions.entityHistoryItem = {
        chain_id: eventIdentifier.chainId,
        block_number: eventIdentifier.blockNumber,
        block_timestamp: eventIdentifier.blockTimestamp,
        log_index: eventIdentifier.logIndex,
        previous_chain_id: mapPrev(prev => prev.chainId),
        previous_block_timestamp: mapPrev(prev => prev.blockTimestamp),
        previous_block_number: mapPrev(prev => prev.blockNumber),
        previous_log_index: mapPrev(prev => prev.logIndex),
        entity_type: entityType,
        entity_id,
        params,
      }
      entityHistoryItems->Belt.Array.concat([historyItem])
    } else {
      entityHistoryItems
    }

    (Some(eventIdentifier), entityHistoryItems)
  })

  entityHistoryItems
}

let executeSetEntityWithHistory = (
  type entity,
  sql: Postgres.sql,
  ~rows: array<Types.inMemoryStoreRowEntity<entity>>,
  ~entityMod: module(Entities.Entity with type t = entity),
): promise<unit> => {
  let module(EntityMod) = entityMod
  let {schema, table} = module(EntityMod)
  let (entitiesToSet, idsToDelete, entityHistoryItemsToSet) = rows->Belt.Array.reduce(
    ([], [], []),
    ((entitiesToSet, idsToDelete, entityHistoryItemsToSet), row) => {
      switch row {
      | Updated({latest, history}) =>
        let entityHistoryItems =
          history
          ->Belt.Array.concat([latest])
          ->getEntityHistoryItems(~entitySchema=schema, ~entityType=table.tableName)

        switch latest.entityUpdateAction {
        | Set(entity) => (
            entitiesToSet->Belt.Array.concat([entity]),
            idsToDelete,
            entityHistoryItemsToSet->Belt.Array.concat([entityHistoryItems]),
          )
        | Delete(entityId) => (
            entitiesToSet,
            idsToDelete->Belt.Array.concat([entityId]),
            entityHistoryItemsToSet->Belt.Array.concat([entityHistoryItems]),
          )
        }
      | _ => (entitiesToSet, idsToDelete, entityHistoryItemsToSet)
      }
    },
  )

  [
    sql->DbFunctions.EntityHistory.batchSet(
      ~entityHistoriesToSet=Belt.Array.concatMany(entityHistoryItemsToSet),
    ),
    if entitiesToSet->Array.length > 0 {
      sql->Entities.batchSet(entitiesToSet, ~entityMod)
    } else {
      Promise.resolve()
    },
    if idsToDelete->Array.length > 0 {
      sql->Entities.batchDelete(idsToDelete, ~entityMod)
    } else {
      Promise.resolve()
    },
  ]
  ->Promise.all
  ->Promise.thenResolve(_ => ())
}

let executeDbFunctionsEntity = (
  type entity,
  sql: Postgres.sql,
  ~rows: array<Types.inMemoryStoreRowEntity<entity>>,
  ~entityMod: module(Entities.Entity with type t = entity),
): promise<unit> => {
  let (entitiesToSet, idsToDelete) = rows->Belt.Array.reduce(([], []), (
    (accumulatedSets, accumulatedDeletes),
    row,
  ) =>
    switch row {
    | Updated({latest: {entityUpdateAction: Set(entity)}}) => (
        Belt.Array.concat(accumulatedSets, [entity]),
        accumulatedDeletes,
      )
    | Updated({latest: {entityUpdateAction: Delete(entityId)}}) => (
        accumulatedSets,
        Belt.Array.concat(accumulatedDeletes, [entityId]),
      )
    | _ => (accumulatedSets, accumulatedDeletes)
    }
  )

  let promises =
    (
      entitiesToSet->Array.length > 0 ? [sql->Entities.batchSet(entitiesToSet, ~entityMod)] : []
    )->Belt.Array.concat(
      idsToDelete->Array.length > 0 ? [sql->Entities.batchDelete(idsToDelete, ~entityMod)] : [],
    )

  promises->Promise.all->Promise.thenResolve(_ => ())
}

let executeBatch = async (sql, ~inMemoryStore: InMemoryStore.t) => {
  let entityDbExecutionComposer = Config.shouldRollbackOnReorg
    ? executeSetEntityWithHistory
    : executeDbFunctionsEntity

  let setEventSyncState = executeSet(
    ~dbFunction=DbFunctions.EventSyncState.batchSet,
    ~items=inMemoryStore.eventSyncState->InMemoryStore.EventSyncState.values,
  )

  let setRawEvents = executeSet(
    ~dbFunction=DbFunctions.RawEvents.batchSet,
    ~items=inMemoryStore.rawEvents->InMemoryStore.RawEvents.values,
  )

  let setDynamicContracts = executeSet(
    ~dbFunction=DbFunctions.DynamicContractRegistry.batchSet,
    ~items=inMemoryStore.dynamicContractRegistry->InMemoryStore.DynamicContractRegistry.values,
  )

  let setEntryPoint_AccountDeployeds = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_AccountDeployed),
    ~rows=inMemoryStore.entryPoint_AccountDeployed->InMemoryStore.EntryPoint_AccountDeployed.values,
  )

  let setEntryPoint_BeforeExecutions = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_BeforeExecution),
    ~rows=inMemoryStore.entryPoint_BeforeExecution->InMemoryStore.EntryPoint_BeforeExecution.values,
  )

  let setEntryPoint_Depositeds = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_Deposited),
    ~rows=inMemoryStore.entryPoint_Deposited->InMemoryStore.EntryPoint_Deposited.values,
  )

  let setEntryPoint_SignatureAggregatorChangeds = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_SignatureAggregatorChanged),
    ~rows=inMemoryStore.entryPoint_SignatureAggregatorChanged->InMemoryStore.EntryPoint_SignatureAggregatorChanged.values,
  )

  let setEntryPoint_StakeLockeds = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_StakeLocked),
    ~rows=inMemoryStore.entryPoint_StakeLocked->InMemoryStore.EntryPoint_StakeLocked.values,
  )

  let setEntryPoint_StakeUnlockeds = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_StakeUnlocked),
    ~rows=inMemoryStore.entryPoint_StakeUnlocked->InMemoryStore.EntryPoint_StakeUnlocked.values,
  )

  let setEntryPoint_StakeWithdrawns = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_StakeWithdrawn),
    ~rows=inMemoryStore.entryPoint_StakeWithdrawn->InMemoryStore.EntryPoint_StakeWithdrawn.values,
  )

  let setEntryPoint_UserOperationEvents = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_UserOperationEvent),
    ~rows=inMemoryStore.entryPoint_UserOperationEvent->InMemoryStore.EntryPoint_UserOperationEvent.values,
  )

  let setEntryPoint_UserOperationRevertReasons = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_UserOperationRevertReason),
    ~rows=inMemoryStore.entryPoint_UserOperationRevertReason->InMemoryStore.EntryPoint_UserOperationRevertReason.values,
  )

  let setEntryPoint_Withdrawns = entityDbExecutionComposer(
    ~entityMod=module(Entities.EntryPoint_Withdrawn),
    ~rows=inMemoryStore.entryPoint_Withdrawn->InMemoryStore.EntryPoint_Withdrawn.values,
  )

  //In the event of a rollback, rollback all meta tables based on the given
  //valid event identifier, where all rows created after this eventIdentifier should
  //be deleted
  let rollbackTables = switch inMemoryStore.rollBackEventIdentifier {
  | Some(eventIdentifier) =>
    [
      DbFunctions.EntityHistory.deleteAllEntityHistoryAfterEventIdentifier,
      DbFunctions.RawEvents.deleteAllRawEventsAfterEventIdentifier,
      DbFunctions.DynamicContractRegistry.deleteAllDynamicContractRegistrationsAfterEventIdentifier,
    ]->Belt.Array.map(fn => fn(~eventIdentifier))
  | None => []
  }

  let res = await sql->Postgres.beginSql(sql => {
    Belt.Array.concat(
      //Rollback tables need to happen first in the traction
      rollbackTables,
      [
        setEventSyncState,
        setRawEvents,
        setDynamicContracts,
        setEntryPoint_AccountDeployeds,
        setEntryPoint_BeforeExecutions,
        setEntryPoint_Depositeds,
        setEntryPoint_SignatureAggregatorChangeds,
        setEntryPoint_StakeLockeds,
        setEntryPoint_StakeUnlockeds,
        setEntryPoint_StakeWithdrawns,
        setEntryPoint_UserOperationEvents,
        setEntryPoint_UserOperationRevertReasons,
        setEntryPoint_Withdrawns,
      ],
    )->Belt.Array.map(dbFunc => sql->dbFunc)
  })

  res
}

module RollBack = {
  exception DecodeError(S.error)
  let rollBack = async (~chainId, ~blockTimestamp, ~blockNumber, ~logIndex) => {
    let reorgData = switch await DbFunctions.sql->DbFunctions.EntityHistory.getRollbackDiff(
      ~chainId,
      ~blockTimestamp,
      ~blockNumber,
    ) {
    | Ok(v) => v
    | Error(exn) =>
      exn
      ->DecodeError
      ->ErrorHandling.mkLogAndRaise(~msg="Failed to get rollback diff from entity history")
    }

    let rollBackEventIdentifier: Types.eventIdentifier = {
      chainId,
      blockTimestamp,
      blockNumber,
      logIndex,
    }

    let inMemStore = InMemoryStore.makeWithRollBackEventIdentifier(Some(rollBackEventIdentifier))

    reorgData->Belt.Array.forEach(e => {
      switch e {
      //Where previousEntity is Some,
      //set the value with the eventIdentifier that set that value initially
      | {
          previousEntity: Some({entity: EntryPoint_AccountDeployedEntity(entity), eventIdentifier}),
          entityId,
        } =>
        inMemStore.entryPoint_AccountDeployed->InMemoryStore.EntryPoint_AccountDeployed.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({entity: EntryPoint_BeforeExecutionEntity(entity), eventIdentifier}),
          entityId,
        } =>
        inMemStore.entryPoint_BeforeExecution->InMemoryStore.EntryPoint_BeforeExecution.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({entity: EntryPoint_DepositedEntity(entity), eventIdentifier}),
          entityId,
        } =>
        inMemStore.entryPoint_Deposited->InMemoryStore.EntryPoint_Deposited.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({
            entity: EntryPoint_SignatureAggregatorChangedEntity(entity),
            eventIdentifier,
          }),
          entityId,
        } =>
        inMemStore.entryPoint_SignatureAggregatorChanged->InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({entity: EntryPoint_StakeLockedEntity(entity), eventIdentifier}),
          entityId,
        } =>
        inMemStore.entryPoint_StakeLocked->InMemoryStore.EntryPoint_StakeLocked.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({entity: EntryPoint_StakeUnlockedEntity(entity), eventIdentifier}),
          entityId,
        } =>
        inMemStore.entryPoint_StakeUnlocked->InMemoryStore.EntryPoint_StakeUnlocked.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({entity: EntryPoint_StakeWithdrawnEntity(entity), eventIdentifier}),
          entityId,
        } =>
        inMemStore.entryPoint_StakeWithdrawn->InMemoryStore.EntryPoint_StakeWithdrawn.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({
            entity: EntryPoint_UserOperationEventEntity(entity),
            eventIdentifier,
          }),
          entityId,
        } =>
        inMemStore.entryPoint_UserOperationEvent->InMemoryStore.EntryPoint_UserOperationEvent.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({
            entity: EntryPoint_UserOperationRevertReasonEntity(entity),
            eventIdentifier,
          }),
          entityId,
        } =>
        inMemStore.entryPoint_UserOperationRevertReason->InMemoryStore.EntryPoint_UserOperationRevertReason.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      | {
          previousEntity: Some({entity: EntryPoint_WithdrawnEntity(entity), eventIdentifier}),
          entityId,
        } =>
        inMemStore.entryPoint_Withdrawn->InMemoryStore.EntryPoint_Withdrawn.set(
          ~entity=Set(entity)->Types.mkEntityUpdate(~eventIdentifier, ~shouldSaveHistory=false),
          ~key=entityId,
        )
      //Where previousEntity is None,
      //delete it with the eventIdentifier of the rollback event
      | {previousEntity: None, entityType: EntryPoint_AccountDeployed, entityId} =>
        inMemStore.entryPoint_AccountDeployed->InMemoryStore.EntryPoint_AccountDeployed.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_BeforeExecution, entityId} =>
        inMemStore.entryPoint_BeforeExecution->InMemoryStore.EntryPoint_BeforeExecution.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_Deposited, entityId} =>
        inMemStore.entryPoint_Deposited->InMemoryStore.EntryPoint_Deposited.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_SignatureAggregatorChanged, entityId} =>
        inMemStore.entryPoint_SignatureAggregatorChanged->InMemoryStore.EntryPoint_SignatureAggregatorChanged.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_StakeLocked, entityId} =>
        inMemStore.entryPoint_StakeLocked->InMemoryStore.EntryPoint_StakeLocked.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_StakeUnlocked, entityId} =>
        inMemStore.entryPoint_StakeUnlocked->InMemoryStore.EntryPoint_StakeUnlocked.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_StakeWithdrawn, entityId} =>
        inMemStore.entryPoint_StakeWithdrawn->InMemoryStore.EntryPoint_StakeWithdrawn.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_UserOperationEvent, entityId} =>
        inMemStore.entryPoint_UserOperationEvent->InMemoryStore.EntryPoint_UserOperationEvent.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_UserOperationRevertReason, entityId} =>
        inMemStore.entryPoint_UserOperationRevertReason->InMemoryStore.EntryPoint_UserOperationRevertReason.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      | {previousEntity: None, entityType: EntryPoint_Withdrawn, entityId} =>
        inMemStore.entryPoint_Withdrawn->InMemoryStore.EntryPoint_Withdrawn.set(
          ~entity=Delete(entityId)->Types.mkEntityUpdate(
            ~eventIdentifier=rollBackEventIdentifier,
            ~shouldSaveHistory=false,
          ),
          ~key=entityId,
        )
      }
    })

    inMemStore
  }
}
