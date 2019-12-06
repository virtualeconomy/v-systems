package vsys.blockchain.state.contract.lock

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.blockchain.block.TestBlock
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract._
import vsys.blockchain.contract.lock.LockContractGen
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract._

class ExecuteLockContractInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with LockContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val lockContract: Gen[Contract] = lockContractGen()
  val tokenContract: Gen[Contract] = tokenContractGen(false)

  val preconditionsAndLockContractInvalidTest: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisLockGen(master, ts)
    contract <- lockContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initLockContractDataStackGen(sysTokenId.arr)
    // register a lock contract only support VSYS
    regContract <- registerLockGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    // lock to ts + 2
    lockVSYS <- lockAssetGen(master, contractId, ts + 2, attach, fee, ts + 2)
    invalidLockVSYS <- lockAssetGen(master, contractId, ts + 1, attach, fee, ts + 2)
    withdrawData = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(1L))
    withdrawInvalidData = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(10001L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdrawInvalidType = Seq(DataType.ContractAccount, DataType.Address, DataType.Timestamp)
    // withdraw, time based on last block timestamp
    withdrawVSYS <- withdrawVSYSGen(master, withdrawData, withdrawType, attach, fee, ts + 2)
    invalidWithdrawVSYS1 <- withdrawVSYSGen(master, withdrawInvalidData, withdrawType, attach, fee, ts + 2)
    invalidWithdrawVSYS2 <- withdrawVSYSGen(master, withdrawData, withdrawInvalidType, attach, fee, ts + 2)
  } yield (genesis, regContract, depositVSYS, lockVSYS, invalidWithdrawVSYS1, invalidWithdrawVSYS2, withdrawVSYS, invalidLockVSYS)

  property("can not withdraw VSYS more than in lock contract") {
    forAll(preconditionsAndLockContractInvalidTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, lockV: ExecuteContractFunctionTransaction, invalid: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg, deposit, lockV))),
        TestBlock.createWithTxStatus(invalid.timestamp + 1, Seq(invalid), TransactionStatus.ContractTokenBalanceInsufficient)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  property("can not withdraw VSYS from lock contract with invalid withdraw data type") {
    forAll(preconditionsAndLockContractInvalidTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, lockV: ExecuteContractFunctionTransaction, _, invalid: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg, deposit, lockV))),
        TestBlock.createWithTxStatus(invalid.timestamp + 1, Seq(invalid), TransactionStatus.ContractDataTypeMismatch)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  property("can not withdraw VSYS from lock contract with time less than locked time") {
    forAll(preconditionsAndLockContractInvalidTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, lockV: ExecuteContractFunctionTransaction, _, _, invalid: ExecuteContractFunctionTransaction, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg, deposit, lockV))),
        TestBlock.createWithTxStatus(invalid.timestamp - 1, Seq(invalid), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("can not update lock VSYS time less than current one") {
    forAll(preconditionsAndLockContractInvalidTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, lockV: ExecuteContractFunctionTransaction, _, _, _, invalid: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg, deposit, lockV))),
        TestBlock.createWithTxStatus(invalid.timestamp + 2, Seq(invalid), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndLockWithTokenInvalidContractTest: Gen[(GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisLockGen(master, ts)
    tContract <- tokenContract
    lContract <- lockContract
    description <- validDescStringGen
    initDataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    regTokenContract <- registerTokenGen(master, tContract, initDataStack, description, fee + 10000000000L, ts)
    tokenContractId = regTokenContract.contractId
    tokenId = tokenIdFromBytes(tokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initLockContractDataStackGen(tokenId.arr)
    // register a lock contract only support regTokenContract's' tokenId
    regContract <- registerLockGen(master, lContract, dataStack, description, fee + 10000000000L, ts + 1)
    lockContractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueToken <- issueTokenGen(master, tokenContractId, 100000L, attach, fee, ts + 1)
    // use token contract deposit/withdraw
    depositData = Seq(master.toAddress.bytes.arr, lockContractId.bytes.arr, Longs.toByteArray(10000L))
    depositInvalidData = Seq(master.toAddress.bytes.arr, lockContractId.bytes.arr, Longs.toByteArray(100001L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositInvalidType = Seq(DataType.Address, DataType.ContractAccount, DataType.Timestamp)
    depositToken <- depositTokenGen(master, tokenContractId, false, depositData, depositType, attach, fee, ts + 2)
    invalidDepositToken1 <- depositTokenGen(master, tokenContractId, false, depositInvalidData, depositType, attach, fee, ts + 2)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 2)
    invalidDepositToken2 <- depositTokenGen(master, tokenContractId, false, depositData, depositInvalidType, attach, fee, ts + 2)
    // lock to ts + 2
    lockToken <- lockAssetGen(master, lockContractId, ts + 3, attach, fee, ts + 3)
    invalidLockToken <- lockAssetGen(master, lockContractId, ts + 2, attach, fee, ts + 3)
    withdrawData = Seq(lockContractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(1L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    // withdraw, time based on last block timestamp
    withdrawToken <- withdrawTokenGen(master, tokenContractId, false, withdrawData, withdrawType, attach, fee, ts + 3)
  } yield (genesis, regContract, regTokenContract, issueToken, depositToken, invalidDepositToken1, invalidDepositToken2, lockToken, invalidLockToken, withdrawToken, depositVSYS)

  property("can not deposit token to lock contract more than owned") {
    forAll(preconditionsAndLockWithTokenInvalidContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2, issue, _, invalid: ExecuteContractFunctionTransaction, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(reg.timestamp, Seq(reg2, reg, issue))),
        TestBlock.createWithTxStatus(invalid.timestamp + 1, Seq(invalid), TransactionStatus.ContractTokenBalanceInsufficient)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  property("can not deposit token to lock contract with invalid deposit data type") {
    forAll(preconditionsAndLockWithTokenInvalidContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2, issue, _, _, invalid: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(reg.timestamp, Seq(reg2, reg, issue))),
        TestBlock.createWithTxStatus(invalid.timestamp + 1, Seq(invalid), TransactionStatus.ContractDataTypeMismatch)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  property("can not deposit token to lock contract with from invalid token/system contract") {
    forAll(preconditionsAndLockWithTokenInvalidContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2, issue, deposit, _, _, lockV: ExecuteContractFunctionTransaction, _, withdraw: ExecuteContractFunctionTransaction, invalid: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)),
        TestBlock.create(lockV.timestamp + 1, Seq(reg2, reg, issue, deposit, lockV)),
        TestBlock.create(withdraw.timestamp + 1, Seq(withdraw))),
        TestBlock.createWithTxStatus(invalid.timestamp + 1, Seq(invalid), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("can not withdraw token from lock contract with time less than locked time") {
    forAll(preconditionsAndLockWithTokenInvalidContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2, issue, deposit, _, _, lockV, _, invalid: ExecuteContractFunctionTransaction, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(reg.timestamp, Seq(reg2, reg, issue, deposit, lockV))),
        TestBlock.createWithTxStatus(invalid.timestamp - 1, Seq(invalid), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("can not update lock token time less than current one") {
    forAll(preconditionsAndLockWithTokenInvalidContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2, issue, deposit, _, _, lockV, invalid: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(reg.timestamp, Seq(reg2, reg, issue, deposit, lockV))),
        TestBlock.createWithTxStatus(invalid.timestamp + 1, Seq(invalid), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

}
