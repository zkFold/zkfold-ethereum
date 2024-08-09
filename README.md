# zkFold: Ethereum integration library
Integration of zkFold Symbolic smart contracts with the Ethereum blockchain. The repository contains
- the smart contract specification;
- user documentation for zkFold Symbolic circuits;
- solidity implementation of the verifier contracts (in progress).

## User documentation for zkFold Symbolic circuits

The user documentation contains the necessary information about the development of ZK smart contracts using zkFold Symbolic in application to the Ethereum blockchain.

### What is zkFold Symbolic?

zkFold Symbolic is a subset of Haskell compilable to arithmetic circuits. It is, therefore, a high-level functional programming language that allows developers to harness the power of zero knowledge protocols for their trustless, decentralized, and privacy-preserving applications. It aims to reduce the barrier to entry in the development of zk-enhanced applications and smart contracts.

### On-chain verifier specifications

zkFold Symbolic smart contracts can be verified on-chain using a ZK verifier contract. Currently, we provide two verifier contracts: _Plonk_ and _Symbolic_. The difference between those on-chain contracts lies in how the public inputs to the ZK verification protocol are defined. Namely, Plonk verifier contract verifies the statement with the public input provided by the user, while Symbolic verifier contract verifies the statement with the public input derived from the state of the contract as well as transaction data.

You can find the Plonk verifier specification [here](https://github.com/zkFold/zkfold-ethereum/tree/main/e2e-test/specification/plonk/main.pdf). Below is the snippet of the Solidity script for the Plonk verifier contract:

```solidity
// Solidity script (minting policy) for verifying computations on-chain.
contract PlonkToken {
  // The token is minted if and only if the Plonk `proof` is valid for the `computation` on the `input` derived from the token name.
  function _mint(bytes memory computation, bytes memory input, bytes memory proof) public {
    // minting
  }
}
```

You can find the Symbolic verifier specification [here](https://github.com/zkFold/zkfold-ethereum/tree/main/e2e-test/specification/symbolic/main.pdf). Below is the snippet of the Solidity script for the Symbolic verifier contract:

```solidity
// Solidity script for verifying a ZkFold Symbolic smart contract on the current transaction.
contract Symbolic {
  function deposit() external payable {
    // ...
  }
  function withdraw(uint256 amount) external {
    // ...
  }
}
```

### Write your own circuit

1) Read the [documentation](https://docs.zkfold.io/) on writing zkFold Symbolic circuits.
1) Write your own circuit with zkFold Symbolic (or import one of the [examples](https://github.com/zkFold/zkfold-base/tree/main/examples)). (Ethereum support is in progress)

An example of a zkFold Symbolic smart contract:
```haskell
batchTransfer ::
    forall context.  Sig context
    => Tx context
    -> Vector 5 (TxOut context, TxOut context, ByteString 256 context)
    -> Bool context
batchTransfer tx transfers =
    let -- Extract the payment credentials and verify the signatures
        pkhs       = fromJust $ toVector @5 $ map (paymentCredential . txoAddress . txiOutput) $ init $ fromVector $ txInputs tx
        condition1 = all (\(pkh, (payment, change, signature)) -> verifySignature pkh (payment, change) signature) $ zip pkhs transfers
        outputs    = zip [0..] . init . fromVector $ txOutputs tx

        -- Extract the payments from the transaction and validate them
        payments   = fromJust $ toVector @5 $ map snd $ filter (\(i, _) -> even @Integer i) outputs

        condition2 = all (\(p', (p, _, _)) -> p' == p) $ zip payments transfers

        -- Extract the changes from the transaction and validate them
        changes    = fromJust $ toVector @5 $ map snd $ filter (\(i, _) -> odd @Integer i) outputs
        condition3 = all (\(c', (_, c, _)) -> c' == c) $ zip changes transfers

    in condition1 && condition2 && condition3
```

### Deploy a zkFold Symbolic smart contract on Ethereum

1) Choose a suitable on-chain verifier such as Plonk or Symbolic. (in progress)
2) Compile the circuit and write it to a file with `compileIO`.
3) Post the zkFold Symbolic smart contract on Ethereum by deploying it as a ZK verifier contract for the circuit. (in progress)

### Use our API to interact with zkFold Symbolic smart contracts

Plonk verifier contract API (in progress):
- `mint`: mints a token if the Plonk proof is valid for the computation on the input derived from the token name.

Symbolic verifier contract API (in progress):
- `deposit`: deposits funds to the contract;
- `withdraw`: withdraws funds from the contract.