// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

interface IVerifier {
    function Verify(bytes calldata proof, uint256[] calldata public_inputs) external view returns (bool);
}