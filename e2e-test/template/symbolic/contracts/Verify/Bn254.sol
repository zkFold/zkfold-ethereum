// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

struct Fr {
    uint256 value;
}

struct G1Point {
    uint256 X;
    uint256 Y;
}

struct G2Point {
    uint256[2] X;
    uint256[2] Y;
}

library Bn254 {
    uint256 constant qMod = 21888242871839275222246405745257275088696311157297823662689037894645226208583;
    uint256 constant rMod = 21888242871839275222246405745257275088548364400416034343698204186575808495617;
    uint256 constant bCoeff = 3;

    function newFr(uint256 fr) internal pure returns (Fr memory) {
        require(fr < rMod);
        return Fr({value: fr});
    }

    function copy(Fr memory self) internal pure returns (Fr memory n) {
        n.value = self.value;
    }

    function assign(Fr memory self, Fr memory other) internal pure {
        self.value = other.value;
    }

    function inverse(Fr memory fr) internal view returns (Fr memory) {
        require(fr.value != 0);
        return pow(fr, rMod - 2);
    }

    function addAssign(Fr memory self, Fr memory other) internal pure {
        self.value = addmod(self.value, other.value, rMod);
    }

    function subAssign(Fr memory self, Fr memory other) internal pure {
        self.value = addmod(self.value, rMod - other.value, rMod);
    }

    function mulAssign(Fr memory self, Fr memory other) internal pure {
        self.value = mulmod(self.value, other.value, rMod);
    }

    function pow(Fr memory self, uint256 power) internal view returns (Fr memory) {
        uint256[6] memory input = [32, 32, 32, self.value, power, rMod];
        uint256[1] memory result;
        bool success;
        assembly {
            success := staticcall(gas(), 0x05, input, 0xc0, result, 0x20)
        }
        require(success);
        return Fr({value: result[0]});
    }

    function newG1Checked(uint256 x, uint256 y) internal pure returns (G1Point memory) {
        if (x == 0 && y == 0) {
            return G1Point(x, y);
        }

        require(x < qMod);
        require(y < qMod);

        uint256 lhs = mulmod(y, y, qMod);
        uint256 rhs = mulmod(x, x, qMod);
        rhs = mulmod(rhs, x, qMod);
        rhs = addmod(rhs, bCoeff, qMod);
        require(lhs == rhs);

        return G1Point(x, y);
    }

    function copyG1(G1Point memory self) internal pure returns (G1Point memory result) {
        result.X = self.X;
        result.Y = self.Y;
    }

    function assignG1(G1Point memory self, G1Point memory other) internal pure {
        self.X = other.X;
        self.Y = other.Y;
    }

    function negate(G1Point memory self) internal pure {
        if (self.Y == 0) {
            require(self.X == 0);
            return;
        }

        self.Y = qMod - self.Y;
    }

    function pointAdd(G1Point memory p1, G1Point memory p2) internal view returns (G1Point memory r) {
        pointAddCommon(p1, p2, r);
        return r;
    }

    function pointAddAssign(G1Point memory p1, G1Point memory p2) internal view {
        pointAddCommon(p1, p2, p1);
    }

    function pointAddCommon(
        G1Point memory p1,
        G1Point memory p2,
        G1Point memory dest
    ) internal view {
        uint256[4] memory input;

        input[0] = p1.X;
        input[1] = p1.Y;
        input[2] = p2.X;
        input[3] = p2.Y;

        bool success = false;
        assembly {
            success := staticcall(gas(), 6, input, 0x80, dest, 0x40)
        }
        require(success);
    }

    function pointSubAssign(G1Point memory p1, G1Point memory p2) internal view {
        if (p2.X == 0 && p2.Y == 0) {
            return;
        } else if (p1.X == 0 && p1.Y == 0) {
            p1.X = p2.X;
            p1.Y = qMod - p2.Y;
            return;
        } else {
            uint256[4] memory input;

            input[0] = p1.X;
            input[1] = p1.Y;
            input[2] = p2.X;
            input[3] = qMod - p2.Y;

            bool success = false;
            assembly {
                success := staticcall(gas(), 6, input, 0x80, p1, 0x40)
            }
            require(success);
        }
    }

    function pointMul(G1Point memory p, Fr memory s) internal view returns (G1Point memory r) {
        pointMulCommon(p, s, r);
        return r;
    }

    function pointMulAssign(G1Point memory p, Fr memory s) internal view {
        pointMulCommon(p, s, p);
    }

    function pointMulCommon(
        G1Point memory p,
        Fr memory s,
        G1Point memory dest
    ) internal view {
        uint256[3] memory input;
        input[0] = p.X;
        input[1] = p.Y;
        input[2] = s.value;
        bool success;
        assembly {
            success := staticcall(gas(), 7, input, 0x60, dest, 0x40)
        }
        require(success);
    }

    function pairing(
        G1Point memory a1,
        G1Point memory b1,
        G2Point memory b2
    ) internal view returns (bool) {
        G1Point[] memory p1 = new G1Point[](2);
        G2Point[] memory p2 = new G2Point[](2);
        p1[0] = a1;
        p1[1] = b1;
        p2[0] = G2Point(
                [
                    0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2,
                    0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed
                ],
                [
                    0x090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b,
                    0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa
                ]
            );
        p2[1] = b2;
        require(p1.length == p2.length);
        uint256 elements = p1.length;
        uint256 inputSize = elements * 6;
        uint256[] memory input = new uint256[](inputSize);
        for (uint256 i = 0; i < elements; i++) {
            input[i * 6 + 0] = p1[i].X;
            input[i * 6 + 1] = p1[i].Y;
            input[i * 6 + 2] = p2[i].X[0];
            input[i * 6 + 3] = p2[i].X[1];
            input[i * 6 + 4] = p2[i].Y[0];
            input[i * 6 + 5] = p2[i].Y[1];
        }
        uint256[1] memory out;
        bool success;
        assembly {
            success := staticcall(gas(), 8, add(input, 0x20), mul(inputSize, 0x20), out, 0x20)
        }
        require(success);
        return out[0] != 0;
    }
}
