open Table
type id = string

//shorthand for punning
let isPrimaryKey = true
let isNullable = true
let isArray = true
let isIndex = true

module type Entity = {
  type t
  let schema: S.schema<t>
  let rowsSchema: S.schema<array<t>>
  let table: Table.table
}

let batchRead = (type entity, ~entityMod: module(Entity with type t = entity)) => {
  let module(EntityMod) = entityMod
  let {table, rowsSchema} = module(EntityMod)
  DbFunctionsEntities.makeReadEntities(~table, ~rowsSchema)
}

let batchSet = (type entity, ~entityMod: module(Entity with type t = entity)) => {
  let module(EntityMod) = entityMod
  let {table, rowsSchema} = module(EntityMod)
  DbFunctionsEntities.makeBatchSet(~table, ~rowsSchema)
}

let batchDelete = (type entity, ~entityMod: module(Entity with type t = entity)) => {
  let module(EntityMod) = entityMod
  let {table} = module(EntityMod)
  DbFunctionsEntities.makeBatchDelete(~table)
}

module EntryPoint_AccountDeployed = {
  @genType
  type t = {
    factory: string,
    id: id,
    paymaster: string,
    sender: string,
    userOpHash: string,
  }

  let schema = S.object((. s) => {
    factory: s.field("factory", S.string),
    id: s.field("id", S.string),
    paymaster: s.field("paymaster", S.string),
    sender: s.field("sender", S.string),
    userOpHash: s.field("userOpHash", S.string),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_AccountDeployed",
    ~fields=[
      mkField("factory", Text),
      mkField("id", Text, ~isPrimaryKey),
      mkField("paymaster", Text),
      mkField("sender", Text),
      mkField("userOpHash", Text),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_BeforeExecution = {
  @genType
  type t = {id: id}

  let schema = S.object((. s) => {
    id: s.field("id", S.string),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_BeforeExecution",
    ~fields=[
      mkField("id", Text, ~isPrimaryKey),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_Deposited = {
  @genType
  type t = {
    account: string,
    id: id,
    totalDeposit: Ethers.BigInt.t,
  }

  let schema = S.object((. s) => {
    account: s.field("account", S.string),
    id: s.field("id", S.string),
    totalDeposit: s.field("totalDeposit", Ethers.BigInt.schema),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_Deposited",
    ~fields=[
      mkField("account", Text),
      mkField("id", Text, ~isPrimaryKey),
      mkField("totalDeposit", Numeric),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_SignatureAggregatorChanged = {
  @genType
  type t = {
    aggregator: string,
    id: id,
  }

  let schema = S.object((. s) => {
    aggregator: s.field("aggregator", S.string),
    id: s.field("id", S.string),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_SignatureAggregatorChanged",
    ~fields=[
      mkField("aggregator", Text),
      mkField("id", Text, ~isPrimaryKey),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_StakeLocked = {
  @genType
  type t = {
    account: string,
    id: id,
    totalStaked: Ethers.BigInt.t,
    unstakeDelaySec: Ethers.BigInt.t,
  }

  let schema = S.object((. s) => {
    account: s.field("account", S.string),
    id: s.field("id", S.string),
    totalStaked: s.field("totalStaked", Ethers.BigInt.schema),
    unstakeDelaySec: s.field("unstakeDelaySec", Ethers.BigInt.schema),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_StakeLocked",
    ~fields=[
      mkField("account", Text),
      mkField("id", Text, ~isPrimaryKey),
      mkField("totalStaked", Numeric),
      mkField("unstakeDelaySec", Numeric),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_StakeUnlocked = {
  @genType
  type t = {
    account: string,
    id: id,
    withdrawTime: Ethers.BigInt.t,
  }

  let schema = S.object((. s) => {
    account: s.field("account", S.string),
    id: s.field("id", S.string),
    withdrawTime: s.field("withdrawTime", Ethers.BigInt.schema),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_StakeUnlocked",
    ~fields=[
      mkField("account", Text),
      mkField("id", Text, ~isPrimaryKey),
      mkField("withdrawTime", Numeric),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_StakeWithdrawn = {
  @genType
  type t = {
    account: string,
    amount: Ethers.BigInt.t,
    id: id,
    withdrawAddress: string,
  }

  let schema = S.object((. s) => {
    account: s.field("account", S.string),
    amount: s.field("amount", Ethers.BigInt.schema),
    id: s.field("id", S.string),
    withdrawAddress: s.field("withdrawAddress", S.string),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_StakeWithdrawn",
    ~fields=[
      mkField("account", Text),
      mkField("amount", Numeric),
      mkField("id", Text, ~isPrimaryKey),
      mkField("withdrawAddress", Text),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_UserOperationEvent = {
  @genType
  type t = {
    actualGasCost: Ethers.BigInt.t,
    actualGasUsed: Ethers.BigInt.t,
    id: id,
    nonce: Ethers.BigInt.t,
    paymaster: string,
    sender: string,
    success: bool,
    userOpHash: string,
  }

  let schema = S.object((. s) => {
    actualGasCost: s.field("actualGasCost", Ethers.BigInt.schema),
    actualGasUsed: s.field("actualGasUsed", Ethers.BigInt.schema),
    id: s.field("id", S.string),
    nonce: s.field("nonce", Ethers.BigInt.schema),
    paymaster: s.field("paymaster", S.string),
    sender: s.field("sender", S.string),
    success: s.field("success", S.bool),
    userOpHash: s.field("userOpHash", S.string),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_UserOperationEvent",
    ~fields=[
      mkField("actualGasCost", Numeric),
      mkField("actualGasUsed", Numeric),
      mkField("id", Text, ~isPrimaryKey),
      mkField("nonce", Numeric),
      mkField("paymaster", Text),
      mkField("sender", Text),
      mkField("success", Boolean),
      mkField("userOpHash", Text),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_UserOperationRevertReason = {
  @genType
  type t = {
    id: id,
    nonce: Ethers.BigInt.t,
    revertReason: string,
    sender: string,
    userOpHash: string,
  }

  let schema = S.object((. s) => {
    id: s.field("id", S.string),
    nonce: s.field("nonce", Ethers.BigInt.schema),
    revertReason: s.field("revertReason", S.string),
    sender: s.field("sender", S.string),
    userOpHash: s.field("userOpHash", S.string),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_UserOperationRevertReason",
    ~fields=[
      mkField("id", Text, ~isPrimaryKey),
      mkField("nonce", Numeric),
      mkField("revertReason", Text),
      mkField("sender", Text),
      mkField("userOpHash", Text),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

module EntryPoint_Withdrawn = {
  @genType
  type t = {
    account: string,
    amount: Ethers.BigInt.t,
    id: id,
    withdrawAddress: string,
  }

  let schema = S.object((. s) => {
    account: s.field("account", S.string),
    amount: s.field("amount", Ethers.BigInt.schema),
    id: s.field("id", S.string),
    withdrawAddress: s.field("withdrawAddress", S.string),
  })

  let rowsSchema = S.array(schema)

  let table = mkTable(
    "EntryPoint_Withdrawn",
    ~fields=[
      mkField("account", Text),
      mkField("amount", Numeric),
      mkField("id", Text, ~isPrimaryKey),
      mkField("withdrawAddress", Text),
      mkField("db_write_timestamp", Timestamp, ~default="CURRENT_TIMESTAMP"),
    ],
  )
}

type entity =
  | EntryPoint_AccountDeployedEntity(EntryPoint_AccountDeployed.t)
  | EntryPoint_BeforeExecutionEntity(EntryPoint_BeforeExecution.t)
  | EntryPoint_DepositedEntity(EntryPoint_Deposited.t)
  | EntryPoint_SignatureAggregatorChangedEntity(EntryPoint_SignatureAggregatorChanged.t)
  | EntryPoint_StakeLockedEntity(EntryPoint_StakeLocked.t)
  | EntryPoint_StakeUnlockedEntity(EntryPoint_StakeUnlocked.t)
  | EntryPoint_StakeWithdrawnEntity(EntryPoint_StakeWithdrawn.t)
  | EntryPoint_UserOperationEventEntity(EntryPoint_UserOperationEvent.t)
  | EntryPoint_UserOperationRevertReasonEntity(EntryPoint_UserOperationRevertReason.t)
  | EntryPoint_WithdrawnEntity(EntryPoint_Withdrawn.t)

type entityName =
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

let entityNameSchema = S.union([
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

let getEntityParamsDecoder = entityName =>
  switch entityName {
  | EntryPoint_AccountDeployed =>
    json =>
      json
      ->S.parseWith(. EntryPoint_AccountDeployed.schema)
      ->Belt.Result.map(decoded => EntryPoint_AccountDeployedEntity(decoded))
  | EntryPoint_BeforeExecution =>
    json =>
      json
      ->S.parseWith(. EntryPoint_BeforeExecution.schema)
      ->Belt.Result.map(decoded => EntryPoint_BeforeExecutionEntity(decoded))
  | EntryPoint_Deposited =>
    json =>
      json
      ->S.parseWith(. EntryPoint_Deposited.schema)
      ->Belt.Result.map(decoded => EntryPoint_DepositedEntity(decoded))
  | EntryPoint_SignatureAggregatorChanged =>
    json =>
      json
      ->S.parseWith(. EntryPoint_SignatureAggregatorChanged.schema)
      ->Belt.Result.map(decoded => EntryPoint_SignatureAggregatorChangedEntity(decoded))
  | EntryPoint_StakeLocked =>
    json =>
      json
      ->S.parseWith(. EntryPoint_StakeLocked.schema)
      ->Belt.Result.map(decoded => EntryPoint_StakeLockedEntity(decoded))
  | EntryPoint_StakeUnlocked =>
    json =>
      json
      ->S.parseWith(. EntryPoint_StakeUnlocked.schema)
      ->Belt.Result.map(decoded => EntryPoint_StakeUnlockedEntity(decoded))
  | EntryPoint_StakeWithdrawn =>
    json =>
      json
      ->S.parseWith(. EntryPoint_StakeWithdrawn.schema)
      ->Belt.Result.map(decoded => EntryPoint_StakeWithdrawnEntity(decoded))
  | EntryPoint_UserOperationEvent =>
    json =>
      json
      ->S.parseWith(. EntryPoint_UserOperationEvent.schema)
      ->Belt.Result.map(decoded => EntryPoint_UserOperationEventEntity(decoded))
  | EntryPoint_UserOperationRevertReason =>
    json =>
      json
      ->S.parseWith(. EntryPoint_UserOperationRevertReason.schema)
      ->Belt.Result.map(decoded => EntryPoint_UserOperationRevertReasonEntity(decoded))
  | EntryPoint_Withdrawn =>
    json =>
      json
      ->S.parseWith(. EntryPoint_Withdrawn.schema)
      ->Belt.Result.map(decoded => EntryPoint_WithdrawnEntity(decoded))
  }

let allTables: array<table> = [
  EntryPoint_AccountDeployed.table,
  EntryPoint_BeforeExecution.table,
  EntryPoint_Deposited.table,
  EntryPoint_SignatureAggregatorChanged.table,
  EntryPoint_StakeLocked.table,
  EntryPoint_StakeUnlocked.table,
  EntryPoint_StakeWithdrawn.table,
  EntryPoint_UserOperationEvent.table,
  EntryPoint_UserOperationRevertReason.table,
  EntryPoint_Withdrawn.table,
]
let schema = Schema.make(allTables)
