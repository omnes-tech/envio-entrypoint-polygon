// TODO: move to `eventFetching`

let entryPointAbi = `
[{"type":"event","name":"AccountDeployed","inputs":[{"name":"userOpHash","type":"bytes32","indexed":true},{"name":"sender","type":"address","indexed":true},{"name":"factory","type":"address","indexed":false},{"name":"paymaster","type":"address","indexed":false}],"anonymous":false},{"type":"event","name":"BeforeExecution","inputs":[],"anonymous":false},{"type":"event","name":"Deposited","inputs":[{"name":"account","type":"address","indexed":true},{"name":"totalDeposit","type":"uint256","indexed":false}],"anonymous":false},{"type":"event","name":"SignatureAggregatorChanged","inputs":[{"name":"aggregator","type":"address","indexed":true}],"anonymous":false},{"type":"event","name":"StakeLocked","inputs":[{"name":"account","type":"address","indexed":true},{"name":"totalStaked","type":"uint256","indexed":false},{"name":"unstakeDelaySec","type":"uint256","indexed":false}],"anonymous":false},{"type":"event","name":"StakeUnlocked","inputs":[{"name":"account","type":"address","indexed":true},{"name":"withdrawTime","type":"uint256","indexed":false}],"anonymous":false},{"type":"event","name":"StakeWithdrawn","inputs":[{"name":"account","type":"address","indexed":true},{"name":"withdrawAddress","type":"address","indexed":false},{"name":"amount","type":"uint256","indexed":false}],"anonymous":false},{"type":"event","name":"UserOperationEvent","inputs":[{"name":"userOpHash","type":"bytes32","indexed":true},{"name":"sender","type":"address","indexed":true},{"name":"paymaster","type":"address","indexed":true},{"name":"nonce","type":"uint256","indexed":false},{"name":"success","type":"bool","indexed":false},{"name":"actualGasCost","type":"uint256","indexed":false},{"name":"actualGasUsed","type":"uint256","indexed":false}],"anonymous":false},{"type":"event","name":"UserOperationRevertReason","inputs":[{"name":"userOpHash","type":"bytes32","indexed":true},{"name":"sender","type":"address","indexed":true},{"name":"nonce","type":"uint256","indexed":false},{"name":"revertReason","type":"bytes","indexed":false}],"anonymous":false},{"type":"event","name":"Withdrawn","inputs":[{"name":"account","type":"address","indexed":true},{"name":"withdrawAddress","type":"address","indexed":false},{"name":"amount","type":"uint256","indexed":false}],"anonymous":false}]
`->Js.Json.parseExn
