# zkFold: Ethereum integration library
Integration of zkFold Symbolic smart contracts with the Ethereum blockchain. The repository contains
- user documentation for zkFold Symbolic circuits;
- the smart contracts specification;
- the off-chain code for transaction building;
- end-to-end tests for the zkFold Symbolic smart contracts.

# User documentation for zkFold Symbolic circuits

The user documentation contains the necessary information about the development of ZK smart contracts using zkFold Symbolic in application to the Ethereum blockchain.

## What is zkFold Symbolic?

zkFold Symbolic is a subset of Haskell compilable to arithmetic circuits. It is, therefore, a high-level functional programming language that allows developers to harness the power of zero knowledge protocols for their trustless, decentralized, and privacy-preserving applications. It aims to reduce the barrier to entry in the development of zk-enhanced applications and smart contracts.

## Write your own circuit

1) Read the [documentation](https://docs.zkfold.io/) on writing zkFold Symbolic circuits.
1) Write your own circuit with zkFold Symbolic (or import one of the [examples](https://github.com/zkFold/zkfold-base/tree/main/examples)). (Ethereum support is in progress)


# The smart contracts specification

- Core Ethereum smart contract
- ZK-powered token standard

## Core Ethereum smart contract

The Core smart contract which is a plonk verifier with pre built setup, created via the gnark. Also called a Verifier contract.

Core verifier contract API:
- `Verify`: verify your proof and public_inputs;

## ZK-powered token standard

zkFold Symbolic smart contract can be verified on-chain using a ZK verifier contract. Currently, we provide token contract _Symbolic_. Namely, Symbolic verifier contract verifies the statement with the public input derived from the state of the contract as well as transaction data.

You can find the Symbolic verifier specification [here](./e2e-test/specification/symbolic/main.pdf). Below is the snippet of the Solidity script for the Symbolic verifier contract:

Symbolic verifier contract API:
- `deposit`: deposits funds to the contract;
- `withdraw`: withdraws funds from the contract;
- `newTransfer`: create transfer with circuit;
- `batchTransfer`: performs batch transfer to accounts.

# Off-chain code for transaction building

- `circuit`: create circuit example;
- `setup`: create setup from circuit;
- `solidity`: create a Verifier from setup;
- `prover`: prove the setup.

## Building the project

### For Haskell backend

This project can be built with Cabal 3.10.2.1 and GHC 9.6.3.

```
cabal build
```

### For Go backend

```
go build
```

### For JS backend

```
npx hardhat node
```

## Deploy a zkFold Symbolic smart contract on Ethereum

1) Choose a suitable on-chain verifier Symbolic.
2) Compile the circuit and write it to a file with `compileIO`.
3) Create a Verifier contract with setup.
4) Post the Verifier smart contract on Ethereum.
5) Post the zkFold Symbolic smart contract on Ethereum.
6) Сalculate the batch transfer.
7) Claim your rewards.
