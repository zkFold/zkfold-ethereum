// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

import "./Verify/Bn254.sol";
import "./Verify/Transcipt.sol";
import "./Verify/InternalVerifier.sol";

contract VerifierWithDeserialize is InternalVerifier {

    using Bn254 for Fr;
    using Bn254 for G1Point;
    using Bn254 for G2Point;

    using Update for Transcript;

    function deserializeProof(uint256 publicInputs, uint256[] memory serializedProof)
        internal
        pure
        returns (Proof memory proof)
    {
        require(serializedProof.length == 24);

        proof.pubInput = publicInputs;

        uint256 j = 0;

        proof.cmA = Bn254.newG1Checked(serializedProof[j], serializedProof[j + 1]);
        j += 2;
        proof.cmB = Bn254.newG1Checked(serializedProof[j], serializedProof[j + 1]);
        j += 2;
        proof.cmC = Bn254.newG1Checked(serializedProof[j], serializedProof[j + 1]);
        j += 2;
        proof.cmZ = Bn254.newG1Checked(serializedProof[j], serializedProof[j + 1]);
        j += 2;

        proof.cmT1 = Bn254.newG1Checked(
            serializedProof[j],
            serializedProof[j + 1]
        );
        j += 2;

        proof.cmT2 = Bn254.newG1Checked(
            serializedProof[j],
            serializedProof[j + 1]
        );
        j += 2;

        proof.cmT3 = Bn254.newG1Checked(
            serializedProof[j],
            serializedProof[j + 1]
        );
        j += 2;


        proof.a_xi = Bn254.newFr(serializedProof[j]);
        j += 1;
        proof.b_xi = Bn254.newFr(serializedProof[j]);
        j += 1;
        proof.c_xi = Bn254.newFr(serializedProof[j]);
        j += 1;

        proof.z_xi = Bn254.newFr(serializedProof[j]);
        j += 1;

        proof.s1_xi = Bn254.newFr(serializedProof[j]);
        j += 1;

        proof.s2_xi = Bn254.newFr(serializedProof[j]);
        j += 1;

        proof.l1_xi_mul = Bn254.newFr(serializedProof[j]);
        j += 1;

        proof.proof1 = Bn254.newG1Checked(serializedProof[j], serializedProof[j + 1]);
        j += 2;

        proof.proof2 = Bn254.newG1Checked(serializedProof[j], serializedProof[j + 1]);
    }

    function verifyCommitments(
        State memory state,
        Proof memory proof,
        Setup memory setup
    ) internal view returns (bool) {
        Fr memory r0 = makeR0(state, proof);
        G1Point memory d = makeD(state, proof, setup);
        G1Point memory f = makeF(state, proof, setup, d);
        G1Point memory e = makeE(state, proof, r0);

        return isValidPairing(state, proof, setup, f, e);
    }

    function verify(Proof memory proof, Setup memory setup) internal view returns (bool) {
        State memory state;

        initState(state, proof, setup);

        return verifyCommitments(state, proof, setup);
    }
}