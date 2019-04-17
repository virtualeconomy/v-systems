package vsys.state.diffs

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.{ByteStr, Portfolio}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.serialization.Deser
import com.wavesplatform.state2.diffs._
import scorex.account.PublicKeyAccount
import vsys.transaction.TransactionStatus
import vsys.contract._
import vsys.transaction.proof.EllipticCurve25519Proof
import vsys.transaction.contract._

class ExecuteContractFunctionTransactionDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with OpcFunction
  with TransactionGen
  with ContractGen
  with StateVar
  with Texture
  with DataStack {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionAndBuildExecuteContract: Gen[(Array[Byte], Array[Byte], Array[Byte], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- languageCodeFromLengthGen("vdds")
    langVer <- languageVersionFromLengthGen(1)
    init <- initFunGen()
    descriptor <- descriptorFullGen()
    stateVar <- stateVarRandomGen()
    textual <- textureRightGen
  } yield (langCode, langVer, init, descriptor, stateVar, textual)

  property("execute contract build doesn't break invariant"){
    forAll(preconditionAndBuildExecuteContract) { case (langCode, langVer, init, descriptor, stateVar, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, textual) shouldBe an[Right[_, _]]
    }
  }

  val languageCode: String = "vdds"
  val languageVersion: Int = 1

  val newValidContractTest: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    user <- accountGen
    ts <- positiveLongGen
    contract <- newValidContractTest
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee + 10000000000L, feeScale, ts).right.get
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    genesis2: GenesisTransaction = GenesisTransaction.create(user, ENOUGH_AMT, -1, ts).right.get
    feeEx: Long <- smallFeeGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    splitData <- splitDataStackGen(1000, 0)
    supersedeData <- supersedeDataStackGen(user.toAddress)
    issueData <- issueDataStackGen(10000L, 0)
    destoryData <- destroyDataStackGen(100L, 0)
    sendData <- sendDataStackGen(master.toAddress, 500L, 0)
    split: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.splitIndex, splitData, descEx, feeEx, feeScale, ts + 1).right.get
    supersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.supersedeIndex, supersedeData, descEx, feeEx, feeScale, ts + 2).right.get
    issue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user, regContract.contractId, FunId.issueIndex, issueData, descEx, feeEx, feeScale, ts + 3).right.get
    destroy: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user, regContract.contractId, FunId.destroyIndex, destoryData, descEx, feeEx, feeScale, ts + 4).right.get
    send: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user, regContract.contractId, FunId.sendIndex, sendData, descEx, feeEx, feeScale, ts + 5).right.get
  } yield (genesis, genesis2, regContract, split, supersede, issue, destroy, send, send.fee)

  property("execute contract function transactions doesn't break invariant") {
    forAll(preconditionsAndExecuteContractTest) { case (genesis, genesis2, reg, split, supersede, issue, destroy, send, feeEx) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg, split, supersede, issue, destroy))), TestBlock.create(Seq(send))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -feeEx
        totalPortfolioDiff.effectiveBalance shouldBe -feeEx
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        val user = EllipticCurve25519Proof.fromBytes(send.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        val contractId = reg.contractId.bytes
        val tokenId = ByteStr(Bytes.concat(contractId.arr, Ints.toByteArray(0)))
        val issuerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))
        val maxKey = ByteStr(Bytes.concat(tokenId.arr, Array(2.toByte)))
        val totalKey = ByteStr(Bytes.concat(tokenId.arr, Array(3.toByte)))
        val unityKey = ByteStr(Bytes.concat(tokenId.arr, Array(4.toByte)))
        val descKey = ByteStr(Bytes.concat(tokenId.arr, Array(5.toByte)))
        val descDE = Deser.serilizeString("init")
        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, user.toAddress.bytes.arr))

        newState.accountTransactionIds(master, 5).size shouldBe 5 // genesis, reg, split, supersede, send
        newState.accountTransactionIds(user, 5).size shouldBe 5 // genesis2, supersede, issue, destory, send
        newState.contractTokens(contractId) shouldBe 1
        newState.contractContent(contractId).get._1 shouldBe 2
        newState.contractContent(contractId).get._2.arr shouldEqual reg.id.arr
        newState.contractContent(contractId).get._3.bytes.arr shouldEqual ContractPermitted.contract.bytes.arr
        newState.contractInfo(issuerKey).get.bytes shouldEqual DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes
        newState.contractInfo(makerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.tokenInfo(maxKey).get.bytes shouldEqual DataEntry(Longs.toByteArray(100000000L), DataType.Amount).bytes
        newState.tokenAccountBalance(totalKey) shouldBe 9900L
        newState.tokenInfo(unityKey).get.bytes shouldEqual DataEntry(Longs.toByteArray(1000L), DataType.Amount).bytes
        newState.tokenInfo(descKey).get.bytes shouldEqual DataEntry.create(descDE, DataType.ShortText).right.get.bytes
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 500L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 9400L
      }
    }
  }

  val newValidContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractWithInvalidData: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    ts <- positiveLongGen
    contract <- newValidContract
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee + 10000000000L, feeScale, ts).right.get
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    feeEx: Long <- smallFeeGen
    rep <- mintingAddressGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    dataForIssueDestorySplit: Seq[DataEntry] <- issueDataStackGen(10000L, 0)
    dataForSend: Seq[DataEntry] <- sendDataStackGen(rep, 100L, 0)
    dataForSupersede: Seq[DataEntry] <- supersedeDataStackGen(rep)
    invalidData = dataForIssueDestorySplit ++ dataForSend
    invalidIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.issueIndex, dataForSend, descEx, feeEx, feeScale, ts + 1000).right.get
    invalidSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.splitIndex, dataForSupersede, descEx, feeEx, feeScale, ts + 1000).right.get
    invalidSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.sendIndex, dataForSupersede, descEx, feeEx, feeScale, ts + 2000).right.get
    invalidSuperSede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.supersedeIndex, dataForIssueDestorySplit, descEx, feeEx, feeScale, ts + 3000).right.get
  } yield (genesis, regContract, invalidIssue, invalidSplit, invalidSend, invalidSuperSede)

  property("execute contract transaction fail with invalid data"){
    forAll(preconditionsAndExecuteContractWithInvalidData) { case (genesis, reg, invalid1, invalid2, invalid3, invalid4) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid1), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid2), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid3), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid4), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }

    }
  }

  val newContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractIssue: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    invalidUser <- accountGen
    ts <- positiveLongGen
    contract <- newContract
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee + 10000000000L, feeScale, ts).right.get
    fee2: Long <- smallFeeGen
    funcIdx: Short <- Gen.const(FunId.issueIndex)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    dataValid1: Seq[DataEntry] <- issueDataStackGen(10000L,0)
    dataValid2: Seq[DataEntry] <- issueDataStackGen(99999999L,0)
    dataFailed: Seq[DataEntry] <- issueDataStackGen(100000001L,0)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    genesis2: GenesisTransaction = GenesisTransaction.create(invalidUser, ENOUGH_AMT, -1, ts).right.get
    issueValid1: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx, dataValid1, description2, fee2, feeScale, ts + 1).right.get
    issueValid2: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx, dataValid2, description2, fee2, feeScale, ts + 2).right.get
    issueFailed1: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx, dataFailed, description2, fee2, feeScale, ts + 3).right.get
    issueFailed2: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(invalidUser, regContract.contractId, funcIdx, dataValid1, description2, fee2, feeScale, ts + 3).right.get
  } yield (genesis, genesis2, regContract, issueValid1, issueValid2, issueFailed1, issueFailed2)

  property("execute contract transaction issue successfully"){
    forAll(preconditionsAndExecuteContractIssue) { case (genesis, _, regContract, issue1, issue2, _,  _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(issue1))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(issue2))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction invalid issue"){
    forAll(preconditionsAndExecuteContractIssue) { case (genesis, genesis2, regContract, issue1, issue2, invalid1, invalid2) =>

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract)), TestBlock.create(Seq(issue1))), TestBlock.createWithTxStatus(Seq(issue2), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      } // total > max

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.createWithTxStatus(Seq(invalid1), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      } // total > max

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis2, regContract))), TestBlock.createWithTxStatus(Seq(invalid2), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      } // invalid issue right
    }
  }

  val newContractSend: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractSend: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractSend
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.sendIndex)
    data2: Seq[DataEntry] <- sendDataStackGen(recipient, 100000L,0)
    invalidData: Seq[DataEntry] <- sendDataStackGen(recipient, 1000000L,0)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale, ts1).right.get
    executeContractSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description2, fee2, feeScale, ts2).right.get
    executeContractSendFailed: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, invalidData, description2, fee2, feeScale, ts2).right.get
  } yield (genesis, regContract, executeContractIssue, executeContractSend, executeContractSendFailed)

  property("execute contract transaction send successfully"){
    forAll(preconditionsAndExecuteContractSend) { case (genesis, regContract, executeContractIssue, executeContractSend, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractSend))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction send failed with insufficient token balance"){
    forAll(preconditionsAndExecuteContractSend) { case (genesis, regContract, executeContractIssue, _, executeContractSendFailed) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractSendFailed), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }

  val newContractSupersede: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractSupersede: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    newIssuer <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractSupersede
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.supersedeIndex)
    data2: Seq[DataEntry] <- supersedeDataStackGen(PublicKeyAccount(newIssuer.publicKey).toAddress)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    genesis1: GenesisTransaction = GenesisTransaction.create(newIssuer, ENOUGH_AMT, -1, ts).right.get
    executeContractSupersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale, ts2).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(newIssuer, regContract.contractId, funcIdx1, data1, description2, fee1, feeScale, ts1).right.get
  } yield(genesis, genesis1, regContract, executeContractIssue, executeContractSupersede, executeContractSupersede.fee)

  property("execute contract transaction supersede successfully") {
    forAll(preconditionsAndExecuteContractSupersede) { case (genesis, genesis1, regContract, executeContractIssue, executeContractSupersede, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.create(Seq(executeContractSupersede))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract, executeContractSupersede))), TestBlock.create(Seq(executeContractIssue))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val newContractSplit: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractSplit: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractSplit
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee2: Long <- smallFeeGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.splitIndex)
    data2: Seq[DataEntry] <- splitDataStackGen(10000L, 0)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description2, fee2, feeScale, ts2).right.get
  } yield(genesis, regContract, executeContractSplit, executeContractSplit.fee)

  property("execute contract transaction split successfully") {
    forAll(preconditionsAndExecuteContractSplit) { case (genesis, regContract, executeContractSplit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractSplit))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenDB.isEmpty shouldBe false
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val newContractDestroy: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractDestroy: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract <- newContractDestroy
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    fee2: Long <- smallFeeGen
    funcIdx2: Short <- Gen.const(FunId.destroyIndex)
    data2: Seq[DataEntry] <- destroyDataStackGen(10000L, 0)
    invalidData: Seq[DataEntry] <- destroyDataStackGen(100001L, 0)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale, ts + 1000000L).right.get
    executeContractDestroy: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description2, fee2, feeScale, ts + 2000000L).right.get
    executeContractDestroyFailed: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, invalidData, description2, fee2, feeScale, ts + 2000000L).right.get
  } yield(genesis, regContract, executeContractIssue, executeContractDestroy, executeContractDestroyFailed)

  property("execute contract transaction destroy successfully") {
    forAll(preconditionsAndExecuteContractDestroy) { case (genesis, regContract, executeContractIssue, executeContractDestroy, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractDestroy))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction invalid destroy") {
    forAll(preconditionsAndExecuteContractDestroy) { case (genesis, regContract, executeContractIssue, _, executeContractDestroyFailed) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractDestroyFailed), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }

  val newContractTransfer: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractTransfer: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractTransfer
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.transferIndex)
    data2: Seq[DataEntry] <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 1000L,0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).right.get
    executeContractTransfer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).right.get
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractTransfer.fee)

  property("execute contract transaction transfer successfully"){
    forAll(preconditionsAndExecuteContractTransfer) { case (genesis, regContract, executeContractIssue, executeContractTransfer, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractTransfer), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }

  val newContractDeposit: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractDeposit: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractDeposit
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.depositIndex)
    data2: Seq[DataEntry] <- depositDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 1000L,0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).right.get
    executeContractDeposit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).right.get
  } yield (genesis, regContract, executeContractIssue, executeContractDeposit, executeContractDeposit.fee)

  property("execute contract transaction deposit successfully"){
    forAll(preconditionsAndExecuteContractDeposit) { case (genesis, regContract, executeContractIssue, executeContractDeposit, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractDeposit), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }

  val newContractWithdraw: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractWithdraw: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractDeposit
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    data2: Seq[DataEntry] <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 10000L,0)
    funcIdx2: Short <- Gen.const(FunId.transferIndex)
    fee3: Long <- smallFeeGen
    feeScale3: Short <- feeScaleGen
    ts3: Long <- positiveLongGen
    funcIdx3: Short <- Gen.const(FunId.withdrawIndex)
    data3: Seq[DataEntry] <- withdrawDataStackGen(recipient, PublicKeyAccount(master.publicKey).toAddress, 1000L,0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).right.get
    executeContractTransfer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).right.get
    executeContractWithdraw: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx3, data3, description1, fee3, feeScale3, ts3).right.get
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractWithdraw, executeContractWithdraw.fee)

  property("execute contract transaction withdraw successfully"){
    forAll(preconditionsAndExecuteContractWithdraw) { case (genesis, regContract, executeContractIssue, _, executeContractWithdraw, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractWithdraw), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }

  val newContractTotalSupply: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractTotalSupply: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractTotalSupply
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.totalSupplyIndex)
    data2: Seq[DataEntry] <- totalSupplyDataStackGen(0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).right.get
    executeContractTotalSupply: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).right.get
  } yield (genesis, regContract, executeContractIssue, executeContractTotalSupply, executeContractTotalSupply.fee)

  property("execute contract transaction totalSupply successfully"){
    forAll(preconditionsAndExecuteContractTotalSupply) { case (genesis, regContract, executeContractIssue, executeContractTotalSupply, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractTotalSupply), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }

  val newContractMaxSupply: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractMaxSupply: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractMaxSupply
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.maxSupplyIndex)
    data2: Seq[DataEntry] <- maxSupplyDataStackGen(0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).right.get
    executeContractMaxSupply: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).right.get
  } yield (genesis, regContract, executeContractIssue, executeContractMaxSupply, executeContractMaxSupply.fee)

  property("execute contract transaction maxSupply successfully"){
    forAll(preconditionsAndExecuteContractMaxSupply) { case (genesis, regContract, executeContractIssue, executeContractMaxSupply, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractMaxSupply), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }

  val newContractBalanceOf: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractBalanceOf: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractBalanceOf
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.balanceOfIndex)
    data2: Seq[DataEntry] <- balanceOfDataStackGen(PublicKeyAccount(master.publicKey).toAddress, 0)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).right.get
    executeContractBalanceOf: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).right.get
  } yield (genesis, regContract, executeContractIssue, executeContractBalanceOf, executeContractBalanceOf.fee)

  property("execute contract transaction balanceOf successfully"){
    forAll(preconditionsAndExecuteContractBalanceOf) { case (genesis, regContract, executeContractIssue, executeContractBalanceOf, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractBalanceOf), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }

  val newContractGetIssuer: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndExecuteContractGetIssuer: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractGetIssuer
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.getIssuerIndex)
    //data2: Seq[DataEntry] <- ()
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).right.get
    executeContractGetIssuer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, Nil, description1, fee2, feeScale2, ts2).right.get
  } yield (genesis, regContract, executeContractIssue, executeContractGetIssuer, executeContractGetIssuer.fee)

  property("execute contract transaction getIssuer successfully"){
    forAll(preconditionsAndExecuteContractGetIssuer) { case (genesis, regContract, executeContractIssue, executeContractGetIssuer, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractGetIssuer), TransactionStatus.ExecuteContractFunctionFailed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.ExecuteContractFunctionFailed
      }
    }
  }
}
