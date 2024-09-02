// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

import "./Verify/Bn254.sol";
import "./Verify/Transcript.sol";
import "./Verify/InternalVerifier.sol";

contract Verifier is InternalVerifier {

    using Bn254 for Fr;
    using Bn254 for G1Point;
    using Bn254 for G2Point;

    using Update for Transcript;

    function deserializeSetup(uint256[] memory serializedSetup)
        internal
        pure
        returns (Setup memory setup)
    {
        require(serializedSetup.length == 25);

        uint256 j = 0;

        setup.n = serializedSetup[j];
        j += 1;
        setup.power = serializedSetup[j];
        j += 1;
        setup.x2 = Bn254.newG2Checked(serializedSetup[j], serializedSetup[j + 1], serializedSetup[j + 2], serializedSetup[j + 3]);
        j += 4;
        setup.omega = Bn254.newFr(serializedSetup[j]);
        j += 1;
        setup.k1 = Bn254.newFr(serializedSetup[j]);
        j += 1;
        setup.k2 = Bn254.newFr(serializedSetup[j]);
        j += 1;

        setup.cmQl = Bn254.newG1Checked(serializedSetup[j], serializedSetup[j + 1]);
        j += 2;

        setup.cmQr = Bn254.newG1Checked(serializedSetup[j], serializedSetup[j + 1]);
        j += 2;

        setup.cmQo = Bn254.newG1Checked(serializedSetup[j], serializedSetup[j + 1]);
        j += 2;

        setup.cmQm = Bn254.newG1Checked(serializedSetup[j], serializedSetup[j + 1]);
        j += 2;

        setup.cmQc = Bn254.newG1Checked(serializedSetup[j], serializedSetup[j + 1]);
        j += 2;

        setup.cmS1 = Bn254.newG1Checked(serializedSetup[j], serializedSetup[j + 1]);
        j += 2;

        setup.cmS2 = Bn254.newG1Checked(serializedSetup[j], serializedSetup[j + 1]);
        j += 2;

        setup.cmS3 = Bn254.newG1Checked(serializedSetup[j], serializedSetup[j + 1]);
    }

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

    function verifyInitial(
        State memory state,
        Proof memory proof,
        Setup memory setup
    ) internal view {
        Transcript memory transcript = Update.newTranscript();

        transcript.updateG1(proof.cmA);
        transcript.updateG1(proof.cmB);
        transcript.updateG1(proof.cmC);

        state.beta = transcript.getChallenge();
        state.gamma = transcript.getChallenge();

        transcript.updateG1(proof.cmZ);
        state.alpha = transcript.getChallenge();

        transcript.updateG1(proof.cmT1);
        transcript.updateG1(proof.cmT2);
        transcript.updateG1(proof.cmT3);

        state.xi = transcript.getChallenge();

        transcript.updateFr(proof.a_xi);
        transcript.updateFr(proof.b_xi);
        transcript.updateFr(proof.c_xi);
        transcript.updateFr(proof.s1_xi);
        transcript.updateFr(proof.s2_xi);
        transcript.updateFr(proof.z_xi);

        state.v = transcript.getChallenge();
        transcript.updateG1(proof.proof1);
        transcript.updateG1(proof.proof2);
        state.u = transcript.getChallenge();

        initState(state, proof, setup);
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

        verifyInitial(state, proof, setup);

        return verifyCommitments(state, proof, setup);
    }
}