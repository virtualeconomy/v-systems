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

  val tokenContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndExecuteContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    user <- accountGen
    ts <- positiveLongGen
    contract <- tokenContract
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
    split: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.splitIndex, splitData, descEx, feeEx, feeScale, ts + 1).explicitGet()
    supersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.supersedeIndex, supersedeData, descEx, feeEx, feeScale, ts + 2).explicitGet()
    issue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user,
      regContract.contractId, FunId.issueIndex, issueData, descEx, feeEx, feeScale, ts + 3).explicitGet()
    destroy: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(user,
      regContract.contractId, FunId.destroyIndex, destoryData, descEx, feeEx, feeScale, ts + 4).explicitGet()
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
  val preconditionsAndExecuteContractWithInvalidData: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
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
    invalidIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.issueIndex, dataForSend, descEx, feeEx, feeScale, ts + 1000).explicitGet()
    invalidSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.splitIndex, dataForSupersede, descEx, feeEx, feeScale, ts + 1000).explicitGet()
    invalidSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.sendIndex, dataForSupersede, descEx, feeEx, feeScale, ts + 2000).explicitGet()
    invalidSuperSede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.supersedeIndex, dataForIssueDestorySplit, descEx, feeEx, feeScale, ts + 3000).explicitGet()
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

  val preconditionsAndExecuteContractTransferDepositWithdraw: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    contract <- tokenContract
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee, feeScale, ts).explicitGet()
    issueFuncIdx: Short <- Gen.const(FunId.issueIndex)
    issueData: Seq[DataEntry] <- issueDataStackGen(100000L)
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, issueFuncIdx, issueData, description, fee, feeScale, ts).explicitGet()
    recipient <- mintingAddressGen
    transferData1: Seq[DataEntry] <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress,
      recipient, 1000L)
    executeContractTransfer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.transferIndex, transferData1, description, fee, feeScale, ts).explicitGet()
    transferData2 <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress,
      regContract.contractId, 1000L)
    executeContractTransfer2: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.transferIndex, transferData2, description, fee, feeScale, ts).explicitGet()
    depositData: Seq[DataEntry] <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress,
      regContract.contractId, 1000L)
    executeContractDeposit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.depositIndex, depositData, description, fee, feeScale, ts).explicitGet()
    withdrawData: Seq[DataEntry] <- transferDataStackGen(regContract.contractId,
      PublicKeyAccount(master.publicKey).toAddress, 0L)
    executeContractWithdraw: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.withdrawIndex, withdrawData, description, fee, feeScale, ts).explicitGet()
    invalidDepositData: Seq[DataEntry] <- depositDataStackGen(PublicKeyAccount(master.publicKey).toAddress,
      recipient, 1000L)
    invalidDeposit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.depositIndex, invalidDepositData, description, fee, feeScale, ts).explicitGet()
    invalidWithdrawdata: Seq[DataEntry] <- withdrawDataStackGen(recipient,
      PublicKeyAccount(master.publicKey).toAddress, 1000L)
    invalidWithdraw: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.withdrawIndex, invalidWithdrawdata, description, fee, feeScale, ts).explicitGet()
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractTransfer2, executeContractDeposit, executeContractWithdraw, invalidDeposit, invalidWithdraw)

  property("execute contract transaction transfer function to address successfully"){
    forAll(preconditionsAndExecuteContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, executeContractTransfer, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractTransfer))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  //TODO: add DepositWithdrawContract
  property("execute contract transaction transfer function to contract successfully"){
    forAll(preconditionsAndExecuteContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, executeContractTransfer2, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractTransfer2), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  //TODO: add DepositWithdrawContract
  property("execute contract transaction deposit function successfully"){
    forAll(preconditionsAndExecuteContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, executeContractDeposit, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.createWithTxStatus(Seq(executeContractDeposit), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  //TODO: add DepositWithdrawContract
  property("execute contract transaction withdraw function successfully"){
    forAll(preconditionsAndExecuteContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, executeContractWithdraw, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractWithdraw), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute contract transaction deposit function invalid data"){
    forAll(preconditionsAndExecuteContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, _, invalidDeposit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  property("execute contract transaction withdraw function invalid data"){
    forAll(preconditionsAndExecuteContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, _, _, invalidWithdraw) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidWithdraw), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }
  
  val preconditionsAndExecuteContractTransaction: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    newIssuer <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    genesis1: GenesisTransaction = GenesisTransaction.create(newIssuer, ENOUGH_AMT, -1, ts).explicitGet()
    contract <- tokenContract
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee, feeScale, ts).explicitGet()
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    supersedeData: Seq[DataEntry] <- supersedeDataStackGen(PublicKeyAccount(newIssuer.publicKey).toAddress)
    executeContractSupersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.supersedeIndex, supersedeData, description, fee, feeScale, ts).explicitGet()
    issueData: Seq[DataEntry] <- issueDataStackGen(100000L)
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.issueIndex, issueData, description, fee, feeScale, ts).explicitGet()
    executeContractIssue1: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(newIssuer,
      regContract.contractId, FunId.issueIndex, issueData, description, fee, feeScale, ts).explicitGet()
    destroyData: Seq[DataEntry] <- destroyDataStackGen(10000L)
    executeContractDestroy: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.destroyIndex, destroyData, description, fee, feeScale, ts).explicitGet()
    splitData: Seq[DataEntry] <- splitDataStackGen(10000L)
    executeContractSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.splitIndex, splitData, description, fee, feeScale, ts).explicitGet()
    recipient <- mintingAddressGen
    sendData: Seq[DataEntry] <- sendDataStackGen(recipient, 100000L)
    executeContractSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.sendIndex, sendData, description, fee, feeScale, ts).explicitGet()
    selfSendData: Seq[DataEntry] <- sendDataStackGen(master.toAddress, 100000L)
    executeContractSelfSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.sendIndex, selfSendData, description, fee, feeScale, ts).explicitGet()
    executeContractTotalSupply: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.totalSupplyIndex, Nil, description, fee, feeScale, ts).explicitGet()
    executeContractMaxSupply: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.maxSupplyIndex, Nil, description, fee, feeScale, ts).explicitGet()
    balanceOfdata: Seq[DataEntry] <- balanceOfDataStackGen(PublicKeyAccount(master.publicKey).toAddress)
    executeContractBalanceOf: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.balanceOfIndex, balanceOfdata, description, fee, feeScale, ts).explicitGet()
    executeContractGetIssuer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.getIssuerIndex, Nil, description, fee, feeScale, ts).explicitGet()
  } yield (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue, executeContractIssue1,
    executeContractDestroy, executeContractSplit, executeContractSend, executeContractSelfSend, executeContractTotalSupply,
    executeContractMaxSupply, executeContractBalanceOf, executeContractGetIssuer, executeContractSelfSend.transactionFee)

  property("execute contract transaction supersede function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue, executeContractIssue1, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.create(Seq(executeContractSupersede))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract, executeContractSupersede))),
        TestBlock.createWithTxStatus(Seq(executeContractIssue), TransactionStatus.ContractInvalidCaller)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract, executeContractSupersede))), TestBlock.create(Seq(executeContractIssue1))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction issue function successfully"){
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractIssue))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction destroy function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, executeContractDestroy, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractDestroy))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction split function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, _, _, _, executeContractSplit, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractSplit))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction send function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, executeContractSend, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractSend))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute contract transaction send function self send successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, _, executeContractSelfSend, _, _, _, _, feeSelfSend) =>
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

  property("execute contract transaction totalSupply function unsupported") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, _, _, executeContractTotalSupply, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractTotalSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute contract transaction maxSupply function unsupported") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, _, _, _, executeContractMaxSupply, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractMaxSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute contract transaction balanceOf function unsupported") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, _, _, _, _, executeContractBalanceOf, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractBalanceOf), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute contract transaction getIssuer function unsupported") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, _, _, _, _, _, executeContractGetIssuer, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractGetIssuer), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  val preconditionsAndExecuteContractTransactionInvalid: Gen[(GenesisTransaction, GenesisTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    master <- accountGen
    newIssuer <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    genesis1: GenesisTransaction = GenesisTransaction.create(newIssuer, ENOUGH_AMT, -1, ts).explicitGet()
    contract <- tokenContract
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, dataStack, description, fee, feeScale, ts).explicitGet()
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    supersedeData: Seq[DataEntry] <- supersedeDataStackGen(PublicKeyAccount(newIssuer.publicKey).toAddress)
    invalidSupersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(newIssuer,
      regContract.contractId, FunId.supersedeIndex, supersedeData, description, fee, feeScale, ts).explicitGet()
    issueData: Seq[DataEntry] <- issueDataStackGen(100000L)
    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.issueIndex, issueData, description, fee, feeScale, ts).explicitGet()
    issueDataInvalid: Seq[DataEntry] <- issueDataStackGen(100000001L)
    invalidIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.issueIndex, issueDataInvalid, description, fee, feeScale, ts).explicitGet()
    destroyDataInvalid: Seq[DataEntry] <- destroyDataStackGen(100001L)
    invalidDestroy: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.destroyIndex, destroyDataInvalid, description, fee, feeScale, ts).explicitGet()
    splitDataInvalid: Seq[DataEntry] <- splitDataStackGen(0L)
    invalidSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.splitIndex, splitDataInvalid, description, fee, feeScale, ts).explicitGet()
    recipient <- mintingAddressGen
    sendDataInvalid: Seq[DataEntry] <- sendDataStackGen(recipient, 1000000L)
    invalidSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      regContract.contractId, FunId.sendIndex, sendDataInvalid, description, fee, feeScale, ts).explicitGet()
  } yield (genesis, genesis1, regContract, invalidSupersede, executeContractIssue, invalidIssue, invalidDestroy, invalidSplit, invalidSend)

  property("execute contract transaction invalid supersede function") {
    forAll(preconditionsAndExecuteContractTransactionInvalid) { case (genesis, genesis1, regContract, invalidSupersede, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.createWithTxStatus(Seq(invalidSupersede), TransactionStatus.ContractInvalidSigner)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidSigner
      }
    }
  }

  property("execute contract transaction invalid issue function"){
    forAll(preconditionsAndExecuteContractTransactionInvalid) { case (genesis, _, regContract, _, _, invalidIssue, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))),
        TestBlock.createWithTxStatus(Seq(invalidIssue), TransactionStatus.ContractTokenMaxExceeded)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenMaxExceeded
      } // total > max
    }
  }

  property("execute contract transaction invalid destroy function") {
    forAll(preconditionsAndExecuteContractTransactionInvalid) { case (genesis, _, regContract, _, executeContractIssue, _, invalidDestroy, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDestroy), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  property("execute contract transaction split function invalid unity") {
    forAll(preconditionsAndExecuteContractTransactionInvalid) { case (genesis, _, regContract, _, _, _, _, invalidSplit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.createWithTxStatus(Seq(invalidSplit), TransactionStatus.ContractInvalidTokenInfo)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidTokenInfo
      }
    }
  }

  property("execute contract transaction send function failed with insufficient token balance") {
    forAll(preconditionsAndExecuteContractTransactionInvalid) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, invalidSend) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidSend), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

}
