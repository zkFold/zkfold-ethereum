// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

import "./Bn254.sol";

struct Transcript {
    bytes32 state0;
    bytes32 state1;
    uint32 challengeCounter;
}

library Update {
    uint256 constant FR_MASK = 0x1fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff;

    uint32 constant DST_0 = 0;
    uint32 constant DST_1 = 1;
    uint32 constant DST_CHALLENGE = 2;

    function newTranscript() internal pure returns (Transcript memory t) {
        t.state0 = bytes32(0);
        t.state1 = bytes32(0);
        t.challengeCounter = 0;
    }

    function updateU256(Transcript memory self, uint256 value) internal pure {
        bytes32 oldState0 = self.state0;
        self.state0 = keccak256(abi.encodePacked(DST_0, oldState0, self.state1, value));
        self.state1 = keccak256(abi.encodePacked(DST_1, oldState0, self.state1, value));
    }

    function updateFr(Transcript memory self, Fr memory value) internal pure {
        updateU256(self, value.value);
    }

    function updateG1(Transcript memory self, G1Point memory p) internal pure {
        updateU256(self, p.X);
        updateU256(self, p.Y);
    }

    function getChallenge(Transcript memory self) internal pure returns (Fr memory challenge) {
        bytes32 query = keccak256(abi.encodePacked(DST_CHALLENGE, self.state0, self.state1, self.challengeCounter));
        self.challengeCounter += 1;
        challenge = Fr({value: uint256(query) & FR_MASK});
    }
}