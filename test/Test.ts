import assert from "assert";
import { 
  TestHelpers,
  EntryPoint_AccountDeployedEntity
} from "generated";
const { MockDb, EntryPoint } = TestHelpers;

describe("EntryPoint contract AccountDeployed event tests", () => {
  // Create mock db
  const mockDb = MockDb.createMockDb();

  // Creating mock for EntryPoint contract AccountDeployed event
  const event = EntryPoint.AccountDeployed.createMockEvent({/* It mocks event fields with default values. You can overwrite them if you need */});

  // Processing the event
  const mockDbUpdated = EntryPoint.AccountDeployed.processEvent({
    event,
    mockDb,
  });

  it("EntryPoint_AccountDeployedEntity is created correctly", () => {
    // Getting the actual entity from the mock database
    let actualEntryPointAccountDeployedEntity = mockDbUpdated.entities.EntryPoint_AccountDeployed.get(
      `${event.transactionHash}_${event.logIndex}`
    );

    // Creating the expected entity
    const expectedEntryPointAccountDeployedEntity: EntryPoint_AccountDeployedEntity = {
      id: `${event.transactionHash}_${event.logIndex}`,
      userOpHash: event.params.userOpHash,
      sender: event.params.sender,
      factory: event.params.factory,
      paymaster: event.params.paymaster,
    };
    // Asserting that the entity in the mock database is the same as the expected entity
    assert.deepEqual(actualEntryPointAccountDeployedEntity, expectedEntryPointAccountDeployedEntity, "Actual EntryPointAccountDeployedEntity should be the same as the expectedEntryPointAccountDeployedEntity");
  });
});
