import { buildModule } from "@nomicfoundation/hardhat-ignition/modules";

export default buildModule("Symbolic", (m) => {
  const l1RewardCreator = '0x5FbDB2315678afecb367f032d93F642f64180aa3';
  const l1Withdraw = '0x5FbDB2315678afecb367f032d93F642f64180aa3';
  const l1Token = m.contract("SymbolicToken", ["Saturn V"]);
  const amountWithdraw = 1000;
  const setup = "../assets/setup";
  const proof = "../assets/proof";
  
  const symbolic = m.contract("Symbolic", [l1RewardCreator, l1Withdraw, l1Token, amountWithdraw, setup], {
    value: 1_000_000_000n, // fee transaction
  });

  const queue = m.contract("Queue", [symbolic]);

  const verifier = m.contract("Verifier", [setup], {
    value: 1_000_000_000n, // send ETH
  });

  m.call(queue, "appendTransaction", [verifier]);

  const computeHash = m.call(queue, "computeHash", []);
  const hash = m.readEventArgument(computeHash, "Transfer", "_hash");

  m.call(symbolic, "commitBatch", [hash]);

  m.call(symbolic, "messageWithProof", [l1RewardCreator, l1Withdraw, amountWithdraw, proof]);

  m.call(symbolic, "withdraw", [verifier, proof]);

  return { symbolic };
});
