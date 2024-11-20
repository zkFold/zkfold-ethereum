// SPDX-License-Identifier: MIT 

pragma solidity >=0.7.0 < 0.9.0;

contract Queue {

    // transaction is appended to the queue.
    event Transaction(
        address sender,
        address target,
        uint256 value,
        uint64 queueIndex
    );

    address symbolic;

    bytes32[] messageQueue;

    uint256 pendingQueueIndex;

    constructor(address _symbolic) {
        symbolic = _symbolic;
    }

    function appendTransaction(
        address _sender,
        address _target,
        uint256 _value
    ) external {
        _queueTransaction(_sender, _target, _value);
    }

    // Internal function to queue a L1 transaction.
    function _queueTransaction(
        address _sender,
        address _target,
        uint256 _value
    ) internal {
        // compute transaction hash
        uint256 _index = messageQueue.length;
        bytes32 _hash = computeHash(_sender, _index, _value, _target);
        messageQueue.push(_hash);

        // emit event
        emit Transaction(_sender, _target, _value, uint64(_index));
    }

    function computeHash(
        address _sender,
        uint256 _index,
        uint256 _value,
        address _target
    ) public pure returns (bytes32) {
        bytes32 hash;
        assembly {
            function store_byte(_ptr, v, is_uint) -> ptr {
                ptr := _ptr
                let len
                for {} gt(v, 0) {} {
                    len := add(len, 1)
                    v := shr(8, v)
                }
                mstore8(ptr, add(len, 0x80))
                ptr := add(ptr, 1)
                mstore(ptr, shl(mul(8, sub(32, len)), v))
                ptr := add(ptr, len)
            }

            function store_address(_ptr, v) -> ptr {
                ptr := _ptr
                mstore8(ptr, 0x94)
                ptr := add(ptr, 1)
                mstore(ptr, shl(96, v))
                ptr := add(ptr, 0x14)
            }

            let start_ptr := add(mload(0x40), 5)
            let ptr := start_ptr
            ptr := store_byte(ptr, _index, 1)
            ptr := store_address(ptr, _target)
            ptr := store_byte(ptr, _value, 1)
            ptr := store_address(ptr, _sender)

            hash := keccak256(start_ptr, sub(ptr, start_ptr))
        }
        return hash;
    }
}