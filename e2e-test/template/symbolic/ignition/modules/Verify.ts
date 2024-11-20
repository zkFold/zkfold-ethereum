import { buildModule } from "@nomicfoundation/hardhat-ignition/modules";

export default buildModule("Verify", (m) => {

  const bn254 = m.library("Bn254");
  const internalVerifier = m.library("InternalVerifier");
  const update = m.library("Update");

  const contractWithLibrary = m.contract("Verify", [], {
    libraries: { Bn254: bn254, InternalVerifier: internalVerifier, Update: update },
  });

  return {
    contractWithLibrary,
  };
});
