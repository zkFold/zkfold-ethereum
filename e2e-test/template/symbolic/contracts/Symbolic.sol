// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

// Uncomment this line to use console.log
// import "hardhat/console.sol";

import {IERC20} from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "./IVerifier.sol";

contract Symbolic {
    // L1 sender tokens
    address l1RewardCreator;

    // L1 receiver return in withdrawal
    address l1Withdraw;

    // L1 token return in withdrawal
    IERC20 l1Token;
    
    // amount return in withdrawal
    uint256 amountWithdraw;

    // messenge with proof
    address messenger;

    mapping(uint256 => bytes32) public committedBatches;

    mapping(uint256 => bytes32) public withdrawRoots;

    // Initialize the transfer
    constructor(
        address _l1RewardCreator,
        address _l1Withdraw,
        address _l1Token,
        uint256 _amountWithdraw,
        address _messenger
    ) {
        l1RewardCreator = _l1RewardCreator;
        l1Withdraw = _l1Withdraw;
        l1Token = IERC20(_l1Token);
        amountWithdraw = _amountWithdraw;
        messenger = _messenger;
    }

    function deposit(address from, address to, uint256 amount, bytes calldata data) external payable {
    }

    function newTransfer(address from_account, IERC20 token, address to_account) external payable {

    }

    // Withdraw receiver funds
    function withdraw(IVerifier verifer, bytes calldata proof, uint256[] calldata public_inputs) external payable {
        // from_account token full_amount to_address
        // PlonkVerifier verifer = new PlonkVerifier();
        if(verifer.Verify(proof, public_inputs)) {
           bool sent = l1Token.transferFrom(l1RewardCreator, l1Withdraw, amountWithdraw);
           require(sent, "Token transfer failed");
        }
    }

    struct MessageProof {
        // The index of the batch where the message belongs to.
        uint256 batchIndex;
        // Concatenation of merkle proof for withdraw merkle trie.
        bytes merkleProof;
    }

    // Relay a message with message proof.
    function messageWithProof(
        address _from,
        address _to,
        uint256 _value,
        bytes memory _message,
        MessageProof memory _proof
    ) external view {
      bytes32 _dataHash = keccak256(abi.encodeWithSignature("message(address,address,uint256,bytes)", _from, _to, _value, _message));
      bytes32 _messageRoot = withdrawRoots[_proof.batchIndex];
      require(verifyProof(_messageRoot, _dataHash, _proof.merkleProof), "Invalid proof");
    }

    function verifyProof(
        bytes32 _root,
        bytes32 _hash,
        bytes memory _proof
    ) internal pure returns (bool) {
        uint256 _length = _proof.length / 32;

        for (uint256 i = 0; i < _length; i++) {
            bytes32 item;
            assembly {
                item := mload(add(add(_proof, 0x20), mul(i, 0x20)))
            }

            assembly {
              mstore(0x00, _hash)
              mstore(0x20, item)
              _hash := keccak256(0x00, 0x40)
            }
        }
        return _hash == _root;
    }

    function commitBatch(
        bytes32 _parentBatchHash,
        uint256 _batchIndex,
        uint256 _messages,
        uint256 _length
    ) external {

        bytes32 _batchHash;
        assembly {
            _batchHash := 0x40
        }
        uint256 batchPtr;

        assembly {
            mstore(add(batchPtr, 1), shl(192, _batchIndex))
            mstore(add(batchPtr, 9), shl(192, _messages))
            mstore(add(batchPtr, 57), _parentBatchHash)
        }

        assembly {
            _batchHash := keccak256(batchPtr, _length)
        }

        committedBatches[_batchIndex] = _batchHash;
    }

}
