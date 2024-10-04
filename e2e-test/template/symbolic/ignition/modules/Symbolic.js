const { buildModule } = require("@nomicfoundation/hardhat-ignition/modules");

module.exports = buildModule("Symbolic", (m) => {
  const l1RewardCreator = '0x5FbDB2315678afecb367f032d93F642f64180aa3';
  const l1Withdraw = '0x5FbDB2315678afecb367f032d93F642f64180aa3';
  const l1Token = m.contract("SymbolicToken", ["Saturn V"]);
  const amountWithdraw = 1000;
  const setup = "../assets/setup";
  const verifier = m.contract("Verify", ["Saturn V"]);
  
  const symbolic = m.contract("SymbolicERC20", [l1RewardCreator, l1Withdraw, l1Token, amountWithdraw, setup, verifier], {
    value: 1_000_000_000n, // send ETH to the constructor
  });

  return { symbolic };
});

/*

export default buildModule("Apollo", (m) => {
  const apollo = m.contract("Rocket", ["Saturn V"]);

  m.call(apollo, "launch", []);

  const existingToken = m.contractAt("Token", "0x...");
  
  const balance = m.staticCall(token, "balanceOf", [address]);

  // Reading values from events
  const transfer = m.call(token, "transfer", [receiver, amount]);
  const value = m.readEventArgument(transfer, "Transfer", "_value");

  const send = m.send("SendingEth", address, 1_000_000n);
  const send = m.send("SendingData", address, undefined, "0x16417104");

  const myLib = m.library("MyLib");
  const myContract = m.contract("MyContract", [], {
    libraries: {
      MyLib: myLib,
    },
  });

  const token = m.contract("Token", ["My Token", "TKN", 18]);

  const receiver = m.contract("Receiver", [], {
    after: [token], // `receiver` is deployed after `token`
  });

  const apollo = m.contract("Rocket", [m.getParameter("name", "Saturn V")]);

  const { token } = m.useModule(TokenModule);

  const owner = m.contract("TokenOwner", [token]);
  m.call(token, "transferOwnership", [owner]);

  const account1 = m.getAccount(1);
  const token = m.contract("Token", ["My Token", "TKN2", 18], { from: account1 });
  
  return { apollo };
});

*/