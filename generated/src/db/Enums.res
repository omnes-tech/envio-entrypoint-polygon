// Graphql Enum Type Variants
type enumType<'a> = {
  name: string,
  variants: array<'a>,
}

let mkEnum = (~name, ~variants) => {
  name,
  variants,
}

module type Enum = {
  type variants
  let enum: enumType<variants>
}

module EventType = {
  type variants =
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

  let name = "EVENT_TYPE"
  let variants = [
    EntryPoint_AccountDeployed,
    EntryPoint_BeforeExecution,
    EntryPoint_Deposited,
    EntryPoint_SignatureAggregatorChanged,
    EntryPoint_StakeLocked,
    EntryPoint_StakeUnlocked,
    EntryPoint_StakeWithdrawn,
    EntryPoint_UserOperationEvent,
    EntryPoint_UserOperationRevertReason,
    EntryPoint_Withdrawn,
  ]
  let enum = mkEnum(~name, ~variants)
}

module ContractType = {
  type variants = | @as("EntryPoint") EntryPoint
  let name = "CONTRACT_TYPE"
  let variants = [EntryPoint]
  let enum = mkEnum(~name, ~variants)
}

module EntityType = {
  type variants =
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
  let name = "ENTITY_TYPE"
  let variants = [
    EntryPoint_AccountDeployed,
    EntryPoint_BeforeExecution,
    EntryPoint_Deposited,
    EntryPoint_SignatureAggregatorChanged,
    EntryPoint_StakeLocked,
    EntryPoint_StakeUnlocked,
    EntryPoint_StakeWithdrawn,
    EntryPoint_UserOperationEvent,
    EntryPoint_UserOperationRevertReason,
    EntryPoint_Withdrawn,
  ]
  let enum = mkEnum(~name, ~variants)
}

let allEnums: array<module(Enum)> = [module(EventType), module(ContractType), module(EntityType)]
//todo move logic into above modules
// Graphql Enum Type Variants
