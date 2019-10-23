package vsys.blockchain.state.diffs

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.blockchain.state.{ByteStr, EitherExt2, LeaseInfo, Portfolio}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.utils.serialization.Deser
import vsys.account.{Address, ContractAccount, PublicKeyAccount}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.blockchain.transaction.contract._

class ExecuteContractFunctionTransactionDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with OpcFunction
  with TransactionGen
  with ContractGen
  with StateVar
  with TextualForm
  with DataStack {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionsAndExecuteContractSystemSend: Gen[(GenesisTransaction, ExecuteContractFunctionTransaction, Long, PublicKeyAccount, Address)] = for {
    master <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    feeScale <- feeScaleGen
    recipient <- mintingAddressGen
    fee: Long <- smallFeeGen
    funcIdx: Short <- Gen.const(ContractSystem.FunId.sysSend)
    data: Seq[DataEntry] <- sendDataStackGen(recipient, 100000L)
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    ts1: Long <- positiveLongGen
    executeContractSystemSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, ContractAccount.systemContractId, funcIdx, data, description, fee, feeScale, ts1).right.get
  } yield (genesis, executeContractSystemSend, fee, master, recipient)

  property("execute contract transaction systemSend successfully") {
    forAll(preconditionsAndExecuteContractSystemSend) { case (genesis, executeContractSystemSend, fee, master, recipient) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(executeContractSystemSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = EllipticCurve25519Proof.fromBytes(executeContractSystemSend.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        val recipientPortfolio = newState.accountPortfolio(recipient)
        val senderPortfolio = newState.accountPortfolio(master)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        recipientPortfolio shouldBe Portfolio(100000, LeaseInfo.empty, Map.empty)
        senderPortfolio shouldBe Portfolio(ENOUGH_AMT-100000-fee, LeaseInfo.empty, Map.empty)
        newState.accountTransactionIds(sender.toAddress, 2, 0)._2.size shouldBe 2 // genesis and transfer
      }
    }
  }

  val preconditionsAndBuildExecuteContract: Gen[(Array[Byte], Array[Byte], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- languageCodeFromLengthGen("vdds")
    langVer <- languageVersionFromLengthGen(1)
    init <- triggerGen()
    descriptor <- descriptorFullGen()
    stateVar <- stateVarRandomGen()
    textual <- textualRightGen
  } yield (langCode, langVer, init, descriptor, stateVar, textual)

  property("execute contract build doesn't break invariant"){
    forAll(preconditionsAndBuildExecuteContract) { case (langCode, langVer, init, descriptor, stateVar, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, textual) shouldBe an[Right[_, _]]
    }
  }

  val languageCode: String = "vdds"
  val languageVersion: Int = 1

  val newValidContractTest: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    user <- accountGen
    ts <- positiveLongGen
    contract <- newValidContractTest
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee + 10000000000L, feeScale, ts).explicitGet()
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    genesis2: GenesisTransaction = GenesisTransaction.create(user, ENOUGH_AMT, -1, ts).explicitGet()
    feeEx: Long <- smallFeeGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    splitData <- splitDataStackGen(1000L)
    supersedeData <- supersedeDataStackGen(user.toAddress)
    issueData <- issueDataStackGen(10000L)
    destoryData <- destroyDataStackGen(100L)
    sendData <- sendDataStackGen(master.toAddress, 500L)
    sendData2 <- sendDataStackGen(user.toAddress, 500L)
    split: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.splitIndex, splitData, descEx, feeEx, feeScale, ts + 1).explicitGet()
    supersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.supersedeIndex, supersedeData, descEx, feeEx, feeScale, ts + 2).explicitGet()
    issue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user, regContract.contractId, FunId.issueIndex, issueData, descEx, feeEx, feeScale, ts + 3).explicitGet()
    destroy: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user, regContract.contractId, FunId.destroyIndex, destoryData, descEx, feeEx, feeScale, ts + 4).explicitGet()
    send: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user, regContract.contractId, FunId.sendIndex, sendData, descEx, feeEx, feeScale, ts + 5).explicitGet()
    selfSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user, regContract.contractId, FunId.sendIndex, sendData2, descEx, feeEx, feeScale, ts + 5).explicitGet()
  } yield (genesis, genesis2, regContract, split, supersede, issue, destroy, send, selfSend, send.transactionFee)

  property("execute contract function transactions doesn't break invariant") {
    forAll(preconditionsAndExecuteContractTest) { case (genesis, genesis2, reg, split, supersede, issue, destroy, send, selfSend, feeEx) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg, split, supersede, issue, destroy))), TestBlock.create(Seq(send, selfSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -(feeEx + feeEx)
        totalPortfolioDiff.effectiveBalance shouldBe -(feeEx + feeEx)
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val user = EllipticCurve25519Proof.fromBytes(send.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val contractId = reg.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val issuerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))
        val maxKey = ByteStr(Bytes.concat(tokenId.arr, Array(0.toByte)))
        val totalKey = ByteStr(Bytes.concat(tokenId.arr, Array(1.toByte)))
        val unityKey = ByteStr(Bytes.concat(tokenId.arr, Array(2.toByte)))
        val descKey = ByteStr(Bytes.concat(tokenId.arr, Array(3.toByte)))
        val descDE = Deser.serilizeString("init")
        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, user.toAddress.bytes.arr))

        newState.accountTransactionIds(master, 5, 0)._2.size shouldBe 5 // genesis, reg, split, supersede, send
        newState.accountTransactionIds(user, 6, 0)._2.size shouldBe 6 // genesis2, supersede, issue, destory, send
        newState.contractTokens(contractId) shouldBe 1
        newState.contractContent(contractId).get._1 shouldBe 2
        newState.contractContent(contractId).get._2.arr shouldEqual reg.id.arr
        newState.contractContent(contractId).get._3.bytes.arr shouldEqual ContractPermitted.contract.bytes.arr
        newState.contractInfo(issuerKey).get.bytes shouldEqual DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes
        newState.contractInfo(makerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.tokenInfo(maxKey).get.bytes shouldEqual DataEntry(Longs.toByteArray(100000000L), DataType.Amount).bytes
        newState.tokenAccountBalance(totalKey) shouldBe 9900L
        newState.tokenInfo(unityKey).get.bytes shouldEqual DataEntry(Longs.toByteArray(1000L), DataType.Amount).bytes
        newState.tokenInfo(descKey).get.bytes shouldEqual DataEntry.create(descDE, DataType.ShortText).explicitGet().bytes
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 500L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 9400L
      }
    }
  }

  val newValidContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractWithInvalidData: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    ts <- positiveLongGen
    contract <- newValidContract
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee + 10000000000L, feeScale, ts).explicitGet()
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    feeEx: Long <- smallFeeGen
    rep <- mintingAddressGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    dataForIssueDestorySplit: Seq[DataEntry] <- issueDataStackGen(10000L)
    dataForSend: Seq[DataEntry] <- sendDataStackGen(rep, 100L)
    dataForSupersede: Seq[DataEntry] <- supersedeDataStackGen(rep)
    invalidData = dataForIssueDestorySplit ++ dataForSend
    invalidIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.issueIndex, dataForSend, descEx, feeEx, feeScale, ts + 1000).explicitGet()
    invalidSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.splitIndex, dataForSupersede, descEx, feeEx, feeScale, ts + 1000).explicitGet()
    invalidSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.sendIndex, dataForSupersede, descEx, feeEx, feeScale, ts + 2000).explicitGet()
    invalidSuperSede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, FunId.supersedeIndex, dataForIssueDestorySplit, descEx, feeEx, feeScale, ts + 3000).explicitGet()
  } yield (genesis, regContract, invalidIssue, invalidSplit, invalidSend, invalidSuperSede)

  property("execute contract transaction fail with invalid data"){
    forAll(preconditionsAndExecuteContractWithInvalidData) { case (genesis, reg, invalid1, invalid2, invalid3, invalid4) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid1), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid2), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid3), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid4), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }

    }
  }

  val newContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractIssue: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    invalidUser <- accountGen
    ts <- positiveLongGen
    contract <- newContract
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee + 10000000000L, feeScale, ts).explicitGet()
    fee2: Long <- smallFeeGen
    funcIdx: Short <- Gen.const(FunId.issueIndex)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    dataValid1: Seq[DataEntry] <- issueDataStackGen(10000L)
    dataValid2: Seq[DataEntry] <- issueDataStackGen(99999999L)
    dataFailed: Seq[DataEntry] <- issueDataStackGen(100000001L)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    genesis2: GenesisTransaction = GenesisTransaction.create(invalidUser, ENOUGH_AMT, -1, ts).explicitGet()
    issueValid1: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx, dataValid1, description2, fee2, feeScale, ts + 1).explicitGet()
    issueValid2: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx, dataValid2, description2, fee2, feeScale, ts + 2).explicitGet()
    issueFailed1: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx, dataFailed, description2, fee2, feeScale, ts + 3).explicitGet()
    issueFailed2: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(invalidUser, regContract.contractId, funcIdx, dataValid1, description2, fee2, feeScale, ts + 3).explicitGet()
  } yield (genesis, genesis2, regContract, issueValid1, issueValid2, issueFailed1, issueFailed2)

  property("execute contract transaction issue successfully"){
    forAll(preconditionsAndExecuteContractIssue) { case (genesis, _, regContract, issue1, issue2, _,  _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(issue1))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(issue2))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction invalid issue"){
    forAll(preconditionsAndExecuteContractIssue) { case (genesis, genesis2, regContract, issue1, issue2, invalid1, invalid2) =>

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract)), TestBlock.create(Seq(issue1))), TestBlock.createWithTxStatus(Seq(issue2), TransactionStatus.ContractTokenMaxExceeded)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenMaxExceeded
      } // total > max

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.createWithTxStatus(Seq(invalid1), TransactionStatus.ContractTokenMaxExceeded)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenMaxExceeded
      } // total > max

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis2, regContract))), TestBlock.createWithTxStatus(Seq(invalid2), TransactionStatus.ContractInvalidCaller)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      } // invalid issue right
    }
  }

  val newContractSend: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractSend: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractSend
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.sendIndex)
    data2: Seq[DataEntry] <- sendDataStackGen(recipient, 100000L)
    data3: Seq[DataEntry] <- sendDataStackGen(master.toAddress, 100000L)
    invalidData: Seq[DataEntry] <- sendDataStackGen(recipient, 1000000L)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale, ts1).explicitGet()
    executeContractSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description2, fee2, feeScale, ts2).explicitGet()
    executeContractSelfSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data3, description2, fee2, feeScale, ts2).explicitGet()
    executeContractSendFailed: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, invalidData, description2, fee2, feeScale, ts2).explicitGet()
  } yield (genesis, regContract, executeContractIssue, executeContractSend, executeContractSelfSend, executeContractSendFailed, executeContractSelfSend.transactionFee)

  property("execute contract transaction send successfully"){
    forAll(preconditionsAndExecuteContractSend) { case (genesis, regContract, executeContractIssue, executeContractSend, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractSend))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction self send successfully"){
    forAll(preconditionsAndExecuteContractSend) { case (genesis, regContract, executeContractIssue, _, executeContractSelfSend, _, feeSelfSend) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractSelfSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = EllipticCurve25519Proof.fromBytes(executeContractSelfSend.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        totalPortfolioDiff.balance shouldBe -feeSelfSend
        totalPortfolioDiff.effectiveBalance shouldBe -feeSelfSend
        val contractId = regContract.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val senderBalanceKey = ByteStr(Bytes.concat(tokenId.arr, sender.toAddress.bytes.arr))
        newState.accountTransactionIds(sender.toAddress, 4, 0)._2.size shouldBe 4 // genesis and payment
        newState.tokenAccountBalance(senderBalanceKey) shouldBe 100000L
      }
    }
  }

  property("execute contract transaction send failed with insufficient token balance"){
    forAll(preconditionsAndExecuteContractSend) { case (genesis, regContract, executeContractIssue, _, _, executeContractSendFailed, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractSendFailed), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  val newContractSupersede: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractSupersede: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    newIssuer <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractSupersede
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.supersedeIndex)
    data2: Seq[DataEntry] <- supersedeDataStackGen(PublicKeyAccount(newIssuer.publicKey).toAddress)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    genesis1: GenesisTransaction = GenesisTransaction.create(newIssuer, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractSupersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale, ts2).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(newIssuer, regContract.contractId, funcIdx1, data1, description2, fee1, feeScale, ts1).explicitGet()
    invalidSupersed: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(newIssuer, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale, ts2).explicitGet()
  } yield(genesis, genesis1, regContract, executeContractIssue, executeContractSupersede, invalidSupersed, executeContractSupersede.transactionFee)

  property("execute contract transaction supersede successfully") {
    forAll(preconditionsAndExecuteContractSupersede) { case (genesis, genesis1, regContract, executeContractIssue, executeContractSupersede, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.create(Seq(executeContractSupersede))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract, executeContractSupersede))), TestBlock.create(Seq(executeContractIssue))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction invalid supersede") {
    forAll(preconditionsAndExecuteContractSupersede) { case (genesis, genesis1, regContract, _, _, invaldSupersede, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.createWithTxStatus(Seq(invaldSupersede), TransactionStatus.ContractInvalidSigner)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidSigner
      }
    }
  }

  val newContractSplit: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractSplit: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractSplit
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee2: Long <- smallFeeGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.splitIndex)
    data2: Seq[DataEntry] <- splitDataStackGen(10000L)
    invalidData: Seq[DataEntry] <- splitDataStackGen(0L)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description2, fee2, feeScale, ts2).explicitGet()
    invalidSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, invalidData, description2, fee2, feeScale, ts2).explicitGet()
  } yield(genesis, regContract, executeContractSplit, invalidSplit, executeContractSplit.transactionFee)

  property("execute contract transaction split successfully") {
    forAll(preconditionsAndExecuteContractSplit) { case (genesis, regContract, executeContractSplit, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractSplit))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction invalid split unity") {
    forAll(preconditionsAndExecuteContractSplit) { case (genesis, regContract, _, invalidUnity, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.createWithTxStatus(Seq(invalidUnity), TransactionStatus.ContractInvalidTokenInfo)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidTokenInfo
      }
    }
  }

  val newContractDestroy: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractDestroy: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract <- newContractDestroy
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    fee2: Long <- smallFeeGen
    funcIdx2: Short <- Gen.const(FunId.destroyIndex)
    data2: Seq[DataEntry] <- destroyDataStackGen(10000L)
    invalidData: Seq[DataEntry] <- destroyDataStackGen(100001L)
    description2 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale, ts + 1000000L).explicitGet()
    executeContractDestroy: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description2, fee2, feeScale, ts + 2000000L).explicitGet()
    executeContractDestroyFailed: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, invalidData, description2, fee2, feeScale, ts + 2000000L).explicitGet()
  } yield(genesis, regContract, executeContractIssue, executeContractDestroy, executeContractDestroyFailed)

  property("execute contract transaction destroy successfully") {
    forAll(preconditionsAndExecuteContractDestroy) { case (genesis, regContract, executeContractIssue, executeContractDestroy, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractDestroy))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction invalid destroy") {
    forAll(preconditionsAndExecuteContractDestroy) { case (genesis, regContract, executeContractIssue, _, executeContractDestroyFailed) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractDestroyFailed), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  val newContractTransfer: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractTransfer: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractTransfer
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.transferIndex)
    data2: Seq[DataEntry] <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 1000L)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).explicitGet()
    executeContractTransfer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).explicitGet()
    // will change to valid when transfer opc open the Contract Account withdraw and deposit
    invalidData = Seq(DataEntry(PublicKeyAccount(master.publicKey).toAddress.bytes.arr, DataType.Address), DataEntry(regContract.contractId.bytes.arr, DataType.ContractAccount), DataEntry(Longs.toByteArray(1000L), DataType.Amount))
    invalidTransfer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, invalidData, description1, fee2, feeScale2, ts2).explicitGet()
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, invalidTransfer, executeContractTransfer.transactionFee)

  property("execute contract transaction transfer successfully"){
    forAll(preconditionsAndExecuteContractTransfer) { case (genesis, regContract, executeContractIssue, executeContractTransfer, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractTransfer))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

//  property("execute contract transaction transfer unsupported deposit to contract account"){
//    forAll(preconditionsAndExecuteContractTransfer) { case (genesis, regContract, executeContractIssue, _, unsupportedWithdraw, _) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(unsupportedWithdraw), TransactionStatus.ContractUnsupportedDeposit)) { blockDiffEi =>
//        blockDiffEi shouldBe an[Right[_, _]]
//        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
//        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedDeposit
//      }
//    }
//  }

  val newContractDeposit: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractDeposit: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractDeposit
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.depositIndex)
    data2: Seq[DataEntry] <- depositDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 1000L)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).explicitGet()
    invalidDeposit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).explicitGet()
    // will change to valid when transfer opc open the Contract Account withdraw and deposit
    invalidData = Seq(DataEntry(PublicKeyAccount(master.publicKey).toAddress.bytes.arr, DataType.Address), DataEntry(regContract.contractId.bytes.arr, DataType.ContractAccount), DataEntry(Longs.toByteArray(1000L), DataType.Amount))
    unsupported: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, invalidData, description1, fee2, feeScale2, ts2).explicitGet()
  } yield (genesis, regContract, executeContractIssue, invalidDeposit, unsupported, invalidDeposit.transactionFee)

//  property("execute contract transaction deposit unsupported"){
//    forAll(preconditionsAndExecuteContractDeposit) { case (genesis, regContract, executeContractIssue, _, unsupported, _) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(unsupported), TransactionStatus.ContractUnsupportedDeposit)) { blockDiffEi =>
//        blockDiffEi shouldBe an[Right[_, _]]
//        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
//        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedDeposit
//      }
//    }
//  }

  property("execute contract transaction invalid deposit"){
    forAll(preconditionsAndExecuteContractDeposit) { case (genesis, regContract, executeContractIssue, invalidDeposit, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  val newContractWithdraw: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractWithdraw: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractDeposit
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    data2: Seq[DataEntry] <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 10000L)
    funcIdx2: Short <- Gen.const(FunId.transferIndex)
    fee3: Long <- smallFeeGen
    feeScale3: Short <- feeScaleGen
    ts3: Long <- positiveLongGen
    funcIdx3: Short <- Gen.const(FunId.withdrawIndex)
    data3: Seq[DataEntry] <- withdrawDataStackGen(recipient, PublicKeyAccount(master.publicKey).toAddress, 1000L)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).explicitGet()
    executeContractTransfer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).explicitGet()
    invalidWithdraw: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx3, data3, description1, fee3, feeScale3, ts3).explicitGet()
    // will change to valid when transfer opc open the Contract Account withdraw and deposit
    invalidData = Seq(DataEntry(regContract.contractId.bytes.arr, DataType.ContractAccount), DataEntry(PublicKeyAccount(master.publicKey).toAddress.bytes.arr, DataType.Address), DataEntry(Longs.toByteArray(1000L), DataType.Amount))
    unsupported: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx3, invalidData, description1, fee2, feeScale2, ts2).explicitGet()
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, invalidWithdraw, unsupported, invalidWithdraw.transactionFee)

//  property("execute contract transaction withdraw unsupported"){
//    forAll(preconditionsAndExecuteContractWithdraw) { case (genesis, regContract, executeContractIssue, _, _, unsupported, _) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(unsupported), TransactionStatus.ContractUnsupportedWithdraw)) { blockDiffEi =>
//        blockDiffEi shouldBe an[Right[_, _]]
//        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
//        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedWithdraw
//      }
//    }
//  }

  property("execute contract transaction invalid withdraw"){
    forAll(preconditionsAndExecuteContractWithdraw) { case (genesis, regContract, executeContractIssue, _, invalidWithdraw, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(invalidWithdraw), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  val newContractTotalSupply: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractTotalSupply: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractTotalSupply
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.totalSupplyIndex)
    data2: Seq[DataEntry] <- emptyDataStackGen()
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).explicitGet()
    executeContractTotalSupply: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).explicitGet()
  } yield (genesis, regContract, executeContractIssue, executeContractTotalSupply, executeContractTotalSupply.transactionFee)

  property("execute contract transaction totalSupply successfully"){
    forAll(preconditionsAndExecuteContractTotalSupply) { case (genesis, regContract, executeContractIssue, executeContractTotalSupply, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractTotalSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  val newContractMaxSupply: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractMaxSupply: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractMaxSupply
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.maxSupplyIndex)
    data2: Seq[DataEntry] <- emptyDataStackGen()
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).explicitGet()
    executeContractMaxSupply: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).explicitGet()
  } yield (genesis, regContract, executeContractIssue, executeContractMaxSupply, executeContractMaxSupply.transactionFee)

  property("execute contract transaction maxSupply unsupported"){
    forAll(preconditionsAndExecuteContractMaxSupply) { case (genesis, regContract, executeContractIssue, executeContractMaxSupply, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractMaxSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  val newContractBalanceOf: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractBalanceOf: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractBalanceOf
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.balanceOfIndex)
    data2: Seq[DataEntry] <- balanceOfDataStackGen(PublicKeyAccount(master.publicKey).toAddress)
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).explicitGet()
    executeContractBalanceOf: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).explicitGet()
  } yield (genesis, regContract, executeContractIssue, executeContractBalanceOf, executeContractBalanceOf.transactionFee)

  property("execute contract transaction balanceOf unsupported"){
    forAll(preconditionsAndExecuteContractBalanceOf) { case (genesis, regContract, executeContractIssue, executeContractBalanceOf, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractBalanceOf), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  val newContractGetIssuer: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractGetIssuer: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContractGetIssuer
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).explicitGet()
    fee1: Long <- smallFeeGen
    feeScale1: Short <- feeScaleGen
    ts1: Long <- positiveLongGen
    funcIdx1: Short <- Gen.const(FunId.issueIndex)
    data1: Seq[DataEntry] <- issueDataStackGen(100000L)
    recipient <- mintingAddressGen
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx2: Short <- Gen.const(FunId.getIssuerIndex)
    //data2: Seq[DataEntry] <- ()
    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).explicitGet()
    executeContractGetIssuer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, Nil, description1, fee2, feeScale2, ts2).explicitGet()
  } yield (genesis, regContract, executeContractIssue, executeContractGetIssuer, executeContractGetIssuer.transactionFee)

  property("execute contract transaction getIssuer unsupported"){
    forAll(preconditionsAndExecuteContractGetIssuer) { case (genesis, regContract, executeContractIssue, executeContractGetIssuer, feeCreate) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractGetIssuer), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }
}
