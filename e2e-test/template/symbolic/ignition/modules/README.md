# Cli scripts

## Transactions

To perform the transactions on local testnet, execute the following 2 scripts.

- `./Verify.ts` to publish Solidity scripts on the blockchain.
- `./Queue.ts` to publish Solidity scripts on the blockchain.
- `./Symbolic.ts` to publish Solidity scripts on the blockchain.

## Batch transfer

The key feature of this new token standard will be batched transfers. Users will be
able to send and receive those tokens for a fraction of the cost of a regular ERC20 transfer.

In the `Symbolic.sol` contract, we implemented batch transfer.
Now our transactions become cheaper when we combine several operations into one.
You will be able to run `Symbolic.ts` and conduct a transaction.
