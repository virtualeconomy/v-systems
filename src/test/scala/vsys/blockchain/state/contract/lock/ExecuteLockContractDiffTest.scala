package vsys.blockchain.state.contract.lock

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
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
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.blockchain.transaction.contract._
import vsys.utils.serialization.Deser

class ExecuteLockContractDiffTest extends PropSpec
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

  val preconditionsAndLockContractTest: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisLockGen(master, ts)
    contract <- lockContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initLockContractDataStackGen(sysTokenId.arr)
    // register a lock contract only support VSYS
    regContract <- registerLockContractGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    // lock to ts + 2
    lockVSYS <- lockAssetGen(master, contractId, ts + 2, attach, fee, ts + 2)
    withdrawData = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(1L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    // withdraw, time based on last block timestamp
    withdrawVSYS <- withdrawVSYSGen(master, withdrawData, withdrawType, attach, fee, ts + 2)
  } yield (genesis, regContract, depositVSYS, lockVSYS, withdrawVSYS, fee)

  property("execute lock contract interact with system contract functions doesn't break invariant") {
    forAll(preconditionsAndLockContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, lockV: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg, deposit, lockV))),
        TestBlock.create(withdraw.timestamp + 1, Seq(withdraw))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val contractId = reg.contractId.bytes
        val VSYSId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val contractTokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        val masterBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val masterLockTimeInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte), DataEntry(masterBytes, DataType.Address).bytes))

        newState.accountTransactionIds(master, 5, 0)._2.size shouldBe 5 // genesis, reg, deposit, lock, withdraw
        // lock contract
        newState.contractContent(contractId).get._1 shouldBe 2
        newState.contractContent(contractId).get._2.arr shouldEqual reg.id.arr
        newState.contractContent(contractId).get._3.bytes.arr shouldEqual ContractLock.contract.bytes.arr
        // lock contract Info
        newState.contractInfo(makerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.contractInfo(contractTokenIdKey).get.bytes shouldEqual DataEntry(VSYSId.arr, DataType.TokenId).bytes
        newState.contractNumInfo(masterBalanceInContractKey) shouldBe 10000L - 1L
        newState.contractInfo(masterLockTimeInContractKey).get.bytes shouldBe DataEntry(Longs.toByteArray(lockV.timestamp), DataType.Timestamp).bytes
        // VSYS balance
        newState.balance(master.toAddress) shouldBe ENOUGH_AMT - 10000L + 1L - 4 * fee - 10000000000L
        newState.balance(reg.contractId) shouldBe 9999L
      }
    }
  }

  property("execute deposit VSYS to lock contract successfully") {
    forAll(preconditionsAndLockContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(reg))),
        TestBlock.create(Seq(deposit))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute lock VSYS in lock contract successfully") {
    forAll(preconditionsAndLockContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, lockV, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(reg, deposit))),
        TestBlock.create(Seq(lockV))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  // re-tested, same as the first one
  property("execute withdraw VSYS from lock contract successfully") {
    forAll(preconditionsAndLockContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, lockV: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg, deposit, lockV))),
        TestBlock.create(withdraw.timestamp + 1, Seq(withdraw))) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe false
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndLockWithTokenContractTest: Gen[(GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisLockGen(master, ts)
    tContract <- tokenContract
    lContract <- lockContract
    description <- validDescStringGen
    initDataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    regTokenContract <- registerTokenContractGen(master, tContract, initDataStack, description, fee + 10000000000L, ts)
    tokenContractId = regTokenContract.contractId
    tokenId = tokenIdFromBytes(tokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initLockContractDataStackGen(tokenId.arr)
    // register a lock contract only support regTokenContract's' tokenId
    regContract <- registerLockContractGen(master, lContract, dataStack, description, fee + 10000000000L, ts + 1)
    lockContractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueToken <- issueTokenGen(master, tokenContractId, 100000L, attach, fee, ts + 1)
    // use token contract deposit/withdraw
    depositData = Seq(master.toAddress.bytes.arr, lockContractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositToken <- depositTokenGen(master, tokenContractId, false, depositData, depositType, attach, fee, ts + 2)
    // lock to ts + 2
    lockToken <- lockAssetGen(master, lockContractId, ts + 3, attach, fee, ts + 3)
    withdrawData = Seq(lockContractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(1L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    // withdraw, time based on last block timestamp
    withdrawToken <- withdrawTokenGen(master, tokenContractId, false, withdrawData, withdrawType, attach, fee, ts + 3)
  } yield (genesis, regContract, regTokenContract, issueToken, depositToken, lockToken, withdrawToken, fee)

  property("execute lock contract interact with token contract functions doesn't break invariant") {
    forAll(preconditionsAndLockWithTokenContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction, reg2: RegisterContractTransaction,
    issue, deposit, lockV: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg2, reg, issue, deposit, lockV))),
        TestBlock.create(withdraw.timestamp + 1, Seq(withdraw))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val contractId = reg.contractId.bytes
        val tokenContractId = reg2.contractId.bytes
        val tokenId = tokenIdFromBytes(tokenContractId.arr, Ints.toByteArray(0)).explicitGet()
        val issuerKey = ByteStr(Bytes.concat(tokenContractId.arr, Array(0.toByte)))
        val tokenMakerKey = ByteStr(Bytes.concat(tokenContractId.arr, Array(1.toByte)))
        val maxKey = ByteStr(Bytes.concat(tokenId.arr, Array(0.toByte)))
        val totalKey = ByteStr(Bytes.concat(tokenId.arr, Array(1.toByte)))
        val unityKey = ByteStr(Bytes.concat(tokenId.arr, Array(2.toByte)))
        val descKey = ByteStr(Bytes.concat(tokenId.arr, Array(3.toByte)))
        val descDE = Deser.serilizeString("init")

        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val contractTokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        val masterBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val masterLockTimeInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte), DataEntry(masterBytes, DataType.Address).bytes))

        val masterTokenBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val contractTokenBalanceKey = ByteStr(Bytes.concat(tokenId.arr, contractId.arr))

        newState.accountTransactionIds(master, 7, 0)._2.size shouldBe 7 // genesis, reg, reg2, issue, deposit, lock, withdraw
        // lock contract
        newState.contractContent(contractId).get._1 shouldBe 2
        newState.contractContent(contractId).get._2.arr shouldEqual reg.id.arr
        newState.contractContent(contractId).get._3.bytes.arr shouldEqual ContractLock.contract.bytes.arr
        // token contract
        newState.contractContent(tokenContractId).get._1 shouldBe 2
        newState.contractContent(tokenContractId).get._2.arr shouldEqual reg2.id.arr
        newState.contractContent(tokenContractId).get._3.bytes.arr shouldEqual ContractPermitted.contractWithoutSplit.bytes.arr
        // lock contract Info
        newState.contractInfo(makerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.contractInfo(contractTokenIdKey).get.bytes shouldEqual DataEntry(tokenId.arr, DataType.TokenId).bytes
        newState.contractNumInfo(masterBalanceInContractKey) shouldBe 10000L - 1L
        newState.contractInfo(masterLockTimeInContractKey).get.bytes shouldBe DataEntry(Longs.toByteArray(lockV.timestamp), DataType.Timestamp).bytes
        // token contract Info
        newState.contractInfo(issuerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.contractInfo(tokenMakerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.tokenInfo(maxKey).get.bytes shouldEqual DataEntry(Longs.toByteArray(100000000L), DataType.Amount).bytes
        newState.tokenAccountBalance(totalKey) shouldBe 100000L
        newState.tokenInfo(unityKey).get.bytes shouldEqual DataEntry(Longs.toByteArray(100L), DataType.Amount).bytes
        newState.tokenInfo(descKey).get.bytes shouldEqual DataEntry.create(descDE, DataType.ShortText).explicitGet().bytes
        // VSYS balance
        newState.balance(master.toAddress) shouldBe ENOUGH_AMT - 6 * fee - 20000000000L
        newState.balance(reg.contractId) shouldBe 0L
        // Token balance
        newState.tokenAccountBalance(masterTokenBalanceKey) shouldBe 100000L - 10000L + 1L
        newState.tokenAccountBalance(contractTokenBalanceKey) shouldBe 10000L - 1L
      }
    }
  }

  property("execute deposit token to lock contract successfully") {
    forAll(preconditionsAndLockWithTokenContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2, issue, deposit, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(reg2, reg, issue))),
        TestBlock.create(Seq(deposit))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute lock token in lock contract successfully") {
    forAll(preconditionsAndLockWithTokenContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2, issue, deposit, lockV, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(reg2, reg, issue, deposit))),
        TestBlock.create(Seq(lockV))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  // re-tested, same as the first one
  property("execute withdraw token from lock contract successfully") {
    forAll(preconditionsAndLockWithTokenContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2, issue, deposit, lockV: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg2, reg, issue, deposit, lockV))),
        TestBlock.create(withdraw.timestamp + 1, Seq(withdraw))) { (blockDiff, _) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe false
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }
}
