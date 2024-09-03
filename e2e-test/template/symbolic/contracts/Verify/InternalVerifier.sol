// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

import "./Bn254.sol";
import "./Transcript.sol";

struct Setup {
    uint256 n;
    uint256 power;
    G2Point x2;
    Fr omega;
    Fr k1;
    Fr k2;
    G1Point cmQl;
    G1Point cmQr;
    G1Point cmQo;
    G1Point cmQm;
    G1Point cmQc;
    G1Point cmS1;
    G1Point cmS2;
    G1Point cmS3;
}

struct Proof {
    uint256 pubInput;
    G1Point cmA;
    G1Point cmB;
    G1Point cmC;
    G1Point cmZ;
    G1Point cmT1;
    G1Point cmT2;
    G1Point cmT3;
    Fr a_xi;
    Fr b_xi;
    Fr c_xi;
    Fr s1_xi;
    Fr s2_xi;
    Fr z_xi;
    Fr l1_xi_mul;
    G1Point proof1;
    G1Point proof2;
}

struct State {
    Fr alpha;
    Fr beta;
    Fr gamma;
    Fr v;
    Fr u;
    Fr xi;
    Fr xi_n;
    Fr xi_m_one;
    Fr lagrange1_xi;
    Fr alphaSquare;
    Fr alphaEvalZOmega;
    Fr betaZeta;
    Fr a_xi_gamma;
    Fr b_xi_gamma;
    Fr c_xi_gamma;
    Fr beta_s1_xi;
    Fr beta_s2_xi;
    Fr gamma_beta_a_s1;
    Fr gamma_beta_b_s2;
}

contract InternalVerifier {
    
    using Bn254 for Fr;
    using Bn254 for G1Point;
    using Bn254 for G2Point;

    function initState(
        State memory state,
        Proof memory proof,
        Setup memory setup
    ) internal view {
        state.xi_n.assign(Bn254.pow(state.xi, setup.power));

        state.xi_m_one.assign(state.xi_n);
        state.xi_m_one.subAssign(Bn254.newFr(1));

        state.alphaSquare.assign(state.alpha);
        state.alphaSquare.mulAssign(state.alpha);

        state.alphaEvalZOmega.assign(state.alpha);
        state.alphaEvalZOmega.mulAssign(proof.z_xi);

        state.betaZeta.assign(state.beta);
        state.betaZeta.mulAssign(state.xi);

        state.a_xi_gamma.assign(proof.a_xi);
        state.a_xi_gamma.addAssign(state.gamma);

        state.b_xi_gamma.assign(proof.b_xi);
        state.b_xi_gamma.addAssign(state.gamma);

        state.c_xi_gamma.assign(proof.c_xi);
        state.c_xi_gamma.addAssign(state.gamma);

        state.beta_s1_xi.assign(state.beta);
        state.beta_s1_xi.mulAssign(proof.s1_xi);

        state.beta_s2_xi.assign(state.beta);
        state.beta_s2_xi.mulAssign(proof.s2_xi);

        state.gamma_beta_a_s1.assign(state.a_xi_gamma);
        state.gamma_beta_a_s1.addAssign(state.beta_s1_xi);
        
        state.gamma_beta_b_s2.assign(state.b_xi_gamma);
        state.gamma_beta_b_s2.addAssign(state.beta_s1_xi);
    }

    function makeR0(
        State memory state,
        Proof memory proof
    ) internal pure returns (Fr memory r0) {
        r0 = Bn254.newFr(proof.pubInput);
        r0.mulAssign(state.lagrange1_xi);

        Fr memory e = Bn254.copy(state.alphaSquare);
        e.mulAssign(state.lagrange1_xi);

        r0.subAssign(e);

        e = Bn254.copy(state.alphaEvalZOmega);
        e.mulAssign(state.alphaEvalZOmega);
        e.mulAssign(state.gamma_beta_a_s1);
        e.mulAssign(state.gamma_beta_b_s2);
        e.mulAssign(state.c_xi_gamma);

        r0.subAssign(e);
    }

    function makeD(
        State memory state,
        Proof memory proof,
        Setup memory setup
    ) internal view returns (G1Point memory d) {
        Fr memory e1 = Bn254.copy(proof.a_xi);
        e1.mulAssign(proof.b_xi);

        d = Bn254.copyG1(setup.cmQm);
        d.pointMulAssign(e1);

        G1Point memory g1 = Bn254.copyG1(setup.cmQl);
        g1.pointMulAssign(proof.a_xi);
        d.pointAddAssign(g1);

        g1.assignG1(setup.cmQr);
        g1.pointMulAssign(proof.b_xi);
        d.pointAddAssign(g1);
        
        g1.assignG1(setup.cmQo);
        g1.pointMulAssign(proof.c_xi);
        d.pointAddAssign(g1);

        d.pointAddAssign(setup.cmQc);

        e1.assign(state.a_xi_gamma);
        e1.addAssign(state.betaZeta);

        e1.mulAssign(state.alpha);

        Fr memory e2 = Bn254.copy(setup.k1);
        e2.mulAssign(state.betaZeta);
        e2.addAssign(state.b_xi_gamma);
        e1.mulAssign(e2);

        e2.assign(setup.k2);
        e2.mulAssign(state.betaZeta);
        e2.addAssign(state.c_xi_gamma);
        e1.mulAssign(e2);

        e2.assign(state.alphaSquare);
        e2.mulAssign(state.lagrange1_xi);
        e1.addAssign(e2);

        e1.addAssign(state.u);

        g1.assignG1(proof.cmZ);
        g1.pointMulAssign(e1);
        d.pointAddAssign(g1);

        e1.assign(state.alphaEvalZOmega);
        e1.mulAssign(state.beta);
        e1.mulAssign(state.gamma_beta_a_s1);
        e1.mulAssign(state.gamma_beta_b_s2);

        g1.assignG1(setup.cmS3);
        g1.pointMulAssign(e1);
        d.pointSubAssign(g1);

        G1Point memory g2 = Bn254.copyG1(proof.cmT2);
        g2.pointMulAssign(state.xi_n);

        e1.assign(state.xi_n);
        e1.mulAssign(state.xi_n);
        g1.assignG1(proof.cmT3);
        g1.pointMulAssign(e1);

        g1.pointAddAssign(g2);
        g1.pointAddAssign(proof.cmT1);

        g1.pointMulAssign(state.xi_m_one);

        d.pointSubAssign(g1);
    }

    function makeF(
        State memory state,
        Proof memory proof,
        Setup memory setup,
        G1Point memory d
    ) internal view returns (G1Point memory f) {
        f = Bn254.copyG1(setup.cmS2);
        f.pointMulAssign(state.v);
        f.pointAddAssign(setup.cmS1);

        f.pointMulAssign(state.v);
        f.pointAddAssign(proof.cmC);

        f.pointMulAssign(state.v);
        f.pointAddAssign(proof.cmB);

        f.pointMulAssign(state.v);
        f.pointAddAssign(proof.cmA);

        f.pointAddAssign(d);
    }

    function makeE(
        State memory state,
        Proof memory proof,
        Fr memory r0
    ) internal view returns (G1Point memory e) {
        Fr memory e1 = Bn254.copy(proof.s2_xi);
        e1.mulAssign(state.v);

        e1.addAssign(proof.s1_xi);
        e1.mulAssign(state.v);

        e1.addAssign(proof.c_xi);
        e1.mulAssign(state.v);

        e1.addAssign(proof.b_xi);
        e1.mulAssign(state.v);

        e1.addAssign(proof.a_xi);
        e1.mulAssign(state.v);

        e1.addAssign(Bn254.inverse(r0));

        e = G1Point(1, 2);
        e.pointMulAssign(e1);
    }

    function isValidPairing(
        State memory state,
        Proof memory proof,
        Setup memory setup,
        G1Point memory f,
        G1Point memory e
    ) internal view returns (bool) {
        G1Point memory p1 = Bn254.copyG1(f);
        p1.pointSubAssign(e);
        p1.pointAddAssign(proof.proof1.pointMul(state.xi));

        Fr memory tmpFr = Bn254.copy(state.xi);
        tmpFr.mulAssign(setup.omega);
        tmpFr.mulAssign(state.u);
        p1.pointAddAssign(proof.proof2.pointMul(tmpFr));

        G1Point memory p2 = proof.proof2.pointMul(state.u);
        p2.pointAddAssign(proof.proof1);
        p2.negate();

        return Bn254.pairing(p1, p2, setup.x2);
    }
}
