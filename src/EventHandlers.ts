/*
 * Please refer to https://docs.envio.dev for a thorough guide on all Envio indexer features
 */
import {
  EntryPointContract,
  EntryPoint_AccountDeployedEntity,
  EntryPoint_BeforeExecutionEntity,
  EntryPoint_DepositedEntity,
  EntryPoint_SignatureAggregatorChangedEntity,
  EntryPoint_StakeLockedEntity,
  EntryPoint_StakeUnlockedEntity,
  EntryPoint_StakeWithdrawnEntity,
  EntryPoint_UserOperationEventEntity,
  EntryPoint_UserOperationRevertReasonEntity,
  EntryPoint_WithdrawnEntity,
} from "generated";

EntryPointContract.AccountDeployed.handler(({ event, context }) => {
  const entity: EntryPoint_AccountDeployedEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    userOpHash: event.params.userOpHash,
    sender: event.params.sender,
    factory: event.params.factory,
    paymaster: event.params.paymaster,
  };

  context.EntryPoint_AccountDeployed.set(entity);
});

EntryPointContract.BeforeExecution.handler(({ event, context }) => {
  const entity: EntryPoint_BeforeExecutionEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
  };

  context.EntryPoint_BeforeExecution.set(entity);
});

EntryPointContract.Deposited.handler(({ event, context }) => {
  const entity: EntryPoint_DepositedEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    account: event.params.account,
    totalDeposit: event.params.totalDeposit,
  };

  context.EntryPoint_Deposited.set(entity);
});

EntryPointContract.SignatureAggregatorChanged.handler(({ event, context }) => {
  const entity: EntryPoint_SignatureAggregatorChangedEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    aggregator: event.params.aggregator,
  };

  context.EntryPoint_SignatureAggregatorChanged.set(entity);
});

EntryPointContract.StakeLocked.handler(({ event, context }) => {
  const entity: EntryPoint_StakeLockedEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    account: event.params.account,
    totalStaked: event.params.totalStaked,
    unstakeDelaySec: event.params.unstakeDelaySec,
  };

  context.EntryPoint_StakeLocked.set(entity);
});

EntryPointContract.StakeUnlocked.handler(({ event, context }) => {
  const entity: EntryPoint_StakeUnlockedEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    account: event.params.account,
    withdrawTime: event.params.withdrawTime,
  };

  context.EntryPoint_StakeUnlocked.set(entity);
});

EntryPointContract.StakeWithdrawn.handler(({ event, context }) => {
  const entity: EntryPoint_StakeWithdrawnEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    account: event.params.account,
    withdrawAddress: event.params.withdrawAddress,
    amount: event.params.amount,
  };

  context.EntryPoint_StakeWithdrawn.set(entity);
});

EntryPointContract.UserOperationEvent.handler(({ event, context }) => {
  const entity: EntryPoint_UserOperationEventEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    userOpHash: event.params.userOpHash,
    sender: event.params.sender,
    paymaster: event.params.paymaster,
    nonce: event.params.nonce,
    success: event.params.success,
    actualGasCost: event.params.actualGasCost,
    actualGasUsed: event.params.actualGasUsed,
  };

  context.EntryPoint_UserOperationEvent.set(entity);
});

EntryPointContract.UserOperationRevertReason.handler(({ event, context }) => {
  const entity: EntryPoint_UserOperationRevertReasonEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    userOpHash: event.params.userOpHash,
    sender: event.params.sender,
    nonce: event.params.nonce,
    revertReason: event.params.revertReason,
  };

  context.EntryPoint_UserOperationRevertReason.set(entity);
});

EntryPointContract.Withdrawn.handler(({ event, context }) => {
  const entity: EntryPoint_WithdrawnEntity = {
    id: `${event.transactionHash}_${event.logIndex}`,
    account: event.params.account,
    withdrawAddress: event.params.withdrawAddress,
    amount: event.params.amount,
  };

  context.EntryPoint_Withdrawn.set(entity);
});
