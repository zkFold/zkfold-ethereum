// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

// Uncomment this line to use console.log
// import "hardhat/console.sol";

import {IERC20} from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "./Verify.sol";

contract SymbolicERC20 {
    // L1 sender tokens
    address l1RewardCreator;

    // L1 receiver return in withdrawal
    address l1Withdraw;

    // L1 token return in withdrawal
    IERC20 l1Token;
    
    // amount return in withdrawal
    uint256 amountWithdraw;

    // setup
    Setup setup;

    // Initialize the transfer
    constructor(
        address _l1RewardCreator,
        address _l1Withdraw,
        address _l1Token,
        uint256 _amountWithdraw,
        uint256[] _setup
    ) {
        l1RewardCreator = _l1RewardCreator;
        l1Withdraw = _l1Withdraw;
        l1Token = IERC20(_l1Token);
        amountWithdraw = _amountWithdraw;
        setup = Verifier.deserializeSetup(_setup);
    }

    // Withdraw receiver funds
    function withdraw(uint256 _publicInput, uint256[] _proof) external payable {
       Proof memory proof = Verifier.deserializeProof(_publicInput, _proof);
       if(Verifier.verify(proof, setup)) {
           bool sent = l1Token.transferFrom(l1RewardCreator, l1Withdraw, amountWithdraw);
           require(sent, "Token transfer failed");
       }
    }
}
