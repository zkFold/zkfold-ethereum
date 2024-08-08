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
1) Write your own circuit with zkFold Symbolic (or import it from [example](https://github.com/zkFold/zkfold-base/tree/main/examples)). (support solidity in progress)

What zkFold Symbolic circuit will look like.
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

2) Compile circuit with `compileIO` to scriptFile.
3) Import circuit to off-chain zkfold-ethereum. (in progress)

### Deploy circuit with our smart contract

1) Choose a suitable smart contract such as a plonk or symbolic. (in progress)

```solidity
// Solidity script (minting policy) for verifying computations on-chain.
contract PlonkToken {
  // The token is minted if and only if the Plonk `proof` is valid for the `computation` on the `input` derived from the token name.
  function _mint(bytes memory computation, bytes memory input, bytes memory proof) public {
    // mining
  }
}
```

2) Compile the circuit with our smart contract. (in progress)
3) Deploy it on the blockchain. (in progress)

### Write your own smart contracts on top of our L1 ecosystem

1) Write your own smart contracts with business logic.
2) Deploy it on the blockchain.
