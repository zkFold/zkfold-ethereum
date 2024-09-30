const { ethers } = require("hardhat");

async function main() {
    console.log('Getting the fun token contract...');

    const l1RewardCreator = '0x5FbDB2315678afecb367f032d93F642f64180aa3';
    const l1Withdraw = '0x5FbDB2315678afecb367f032d93F642f64180aa3';

    const amountWithdraw = 1000;
    const setup = "../assets/setup";

    const verifierAddress = '0x3FbDB2315678afecb367f032d93F642f64180123';
    const verifier = await ethers.getContractAt('Verify', verifierAddress);

    const l1TokenAddress = '0x1FdDB2315678afecb367f032d93F642f64180bb2';
    const l1Token = await ethers.getContractAt('SymbolicToken', l1TokenAddress);

    const symbolicAddress = '0x5FbDB2315128afecb367f032d93F642f64180aa3';
    const symbolic = await ethers.getContractAt('SymbolicERC20', symbolicAddress);
}

main()
    .then(() => process.exit(0))
    .catch((error) => {
        console.error(error);
        process.exitCode = 1;
    });