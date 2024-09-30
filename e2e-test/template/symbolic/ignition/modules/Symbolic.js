const { buildModule } = require("@nomicfoundation/hardhat-ignition/modules");

module.exports = buildModule("LockModule", (m) => {
  const symbolic = m.contract("Factory");

  return { symbolic };
});
