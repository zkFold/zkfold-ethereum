# zkFold: Ethereum integration library
Integration of zkFold Symbolic smart contracts with the Ethereum blockchain. The repository contains
- the smart contract specification;
- user documentation for zkFold Symbolic circuits;

## Smart contract specification

The smart contract specification defines the intended behavior of the on-chain code, namely the zkFold Symbolic verifier contract.

[specification](https://github.com/zkFold/zkfold-ethereum/tree/init-docs/e2e-test/specification)

## User documentation for zkFold Symbolic circuits

The user documentation contains the necessary information about the development of ZK smart contracts using zkFold Symbolic in application to Ethereum blockchain.

### Write your own circuit

1) Read [documentation](https://docs.zkfold.io/) on writing circuits on zkFold Symbolic.
1) Write your own circuit with zkFold Symbolic (or import it from [example](https://github.com/zkFold/zkfold-base/tree/main/examples)).
2) Compile circuit with `compileIO` to scriptFile.
3) Import circuit to off-chain zkfold-ethereum. (in progress)

### Deploy circuit with our smart contract

1) Choose a suitable smart contract such as a plonk or symbolic.
2) Compile the schema with our smart contract.
3) Deploy it on the blockchain.

### Write your own smart contracts on top of our L1 ecosystem

1) Write your own smart contracts with business logic.
2) Deploy it on the blockchain.
