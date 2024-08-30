// SPDX-License-Identifier: MIT

pragma solidity >=0.7.0 < 0.9.0;

// Uncomment this line to use console.log
// import "hardhat/console.sol";

import {IERC20} from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

contract SymboicERC20 {
    event PlonkToken(
        address indexed from,
        address l1Token,
        uint256 amount,
        uint256 indexed chainId
    );

    // Stores the hash of the L2 deposit
    bytes32 l2TxDeposit;

    // Maps token balances for each chain to prevent unauthorized spending across sidechain.
    mapping(uint256 chainId => mapping(address l1Token => uint256 balance)) internal chainBalance;

    // L1 receiver return in finalize withdrawal
    address l1Withdraw;

    // L1 token return in finalize withdrawal
    address l1Token;
    
    // amount return in finalize withdrawal
    uint256 amountWithdraw;

    // Initialize the L2 deposit
    constructor(bytes32 _l2TxDeposit) {
        l2TxDeposit = _l2TxDeposit;
    }

    // Withdraw receiver funds
    function withdraw(uint256 _amount,  address _l1Token, address _l1Receiver) external {
        amountWithdraw = _amount;
        l1Token        = _l1Token;
        l1Withdraw     = _l1Receiver;
    }

    // Initiates a deposit by locking funds on the contract and sending the request
    // of processing an L2 transaction where tokens would be minted
    function deposit(
        address _msgSender,
        address _l2Receiver,
        address _l1Token,
        uint256 _amount,
        address _refundRecipient
    ) external payable returns (bytes32 txHash) {
        txHash = l2TxDeposit;
    }

    // Finalize the withdrawal and release funds
    function finalizeWithdrawal(
        uint256            _l2BatchNumber,
        uint256            _l2MessageIndex,
        uint16             _l2TxNumberInBatch,
        bytes calldata     _message,
        bytes32[] calldata _proof
    ) external view returns (address l1Receiver, address l1Token, uint256 amount) {
        l1Receiver = l1Withdraw;
        l1Token = l1Token;
        amount = amountWithdraw;
    }

    // Initiates a deposit by locking funds into a contract and
    // sending a request to process an L2 transaction that will mint tokens
    function depositBridge(
        uint256 _chainId,
        address _message,
        address _l1Token,
        uint256 _amount
    ) external payable {
        if (_l1Token == address(1)) {
            require(msg.value == _amount, "value not equal to amount");
        } else {
            uint256 amount = _deposit(_message, IERC20(_l1Token), _amount);
            require(amount == _amount, "different amount");
        }

        chainBalance[_chainId][_l1Token] += _amount;

        emit PlonkToken(_message, _l1Token, _amount, _chainId);
    }

    // Transfers tokens from the depositor address to the shared bridge address.
    function _deposit(address _from, IERC20 _token, uint256 _amount) internal returns (uint256) {
        uint256 beforeBalance = _token.balanceOf(address(this));
        _token.transferFrom(_from, address(this), _amount);
        uint256 afterBalance = _token.balanceOf(address(this));

        return afterBalance - beforeBalance;
    }
}
