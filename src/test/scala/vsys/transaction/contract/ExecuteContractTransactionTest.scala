package vsys.transaction.contract

import com.wavesplatform.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import com.wavesplatform.state2.diffs._
import scorex.account.PublicKeyAccount
import vsys.transaction.TransactionStatus
import vsys.contract._

class ExecuteContractTransactionTest extends PropSpec
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

//  val newContractTransfer: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
//  val preconditionsAndExecuteContractTransfer: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
//    master <- accountGen
//    ts <- positiveIntGen
//    contract1 <- newContractTransfer
//    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
//    description <- validDescStringGen
//    fee <- smallFeeGen
//    feeScale <- feeScaleGen
//    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
//    fee1: Long <- smallFeeGen
//    feeScale1: Short <- feeScaleGen
//    ts1: Long <- positiveLongGen
//    funcIdx1: Short <- Gen.const(FunId.issueIndex)
//    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
//    recipient <- mintingAddressGen
//    fee2: Long <- smallFeeGen
//    feeScale2: Short <- feeScaleGen
//    ts2: Long <- positiveLongGen
//    funcIdx2: Short <- Gen.const(FunId.transferIndex)
//    data2: Seq[DataEntry] <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 1000L,0)
//    description1 <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
//    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description1, fee1, feeScale1, ts1).right.get
//    executeContractTransfer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description1, fee2, feeScale2, ts2).right.get
//  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractTransfer.fee)
//
//  property("execute contract transaction transfer successfully"){
//    forAll(preconditionsAndExecuteContractTransfer) { case (genesis, regContract, executeContractIssue, executeContractTransfer, feeCreate) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractTransfer))) { blockDiffEi =>
//        blockDiffEi shouldBe an[Right[_, _]]
//        blockDiffEi.toOption.get.txsDiff.tokenAccountBalance.isEmpty shouldBe false
//        blockDiffEi.toOption.get.txsDiff.txStatus shouldBe TransactionStatus.Success
//      }
//    }
//  }

//  val newContractDeposit: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
//  val preconditionsAndExecuteContractDeposit: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
//    master <- accountGen
//    ts <- positiveIntGen
//    contract1 <- newContractDeposit
//    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
//    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
//    fee <- smallFeeGen
//    feeScale <- feeScaleGen
//    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
//    fee1: Long <- smallFeeGen
//    feeScale1: Short <- feeScaleGen
//    ts1: Long <- positiveLongGen
//    funcIdx1: Short <- Gen.const(FunId.issueIndex)
//    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
//    recipient <- mintingAddressGen
//    fee2: Long <- smallFeeGen
//    feeScale2: Short <- feeScaleGen
//    ts2: Long <- positiveLongGen
//    funcIdx2: Short <- Gen.const(FunId.depositIndex)
//    data2: Seq[DataEntry] <- depositDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 1000L,0)
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
//    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description, fee1, feeScale1, ts1).right.get
//    executeContractDeposit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description, fee2, feeScale2, ts2).right.get
//  } yield (genesis, regContract, executeContractIssue, executeContractDeposit, executeContractDeposit.fee)
//
//  property("execute contract transaction deposit successfully"){
//    forAll(preconditionsAndExecuteContractDeposit) { case (genesis, regContract, executeContractIssue, executeContractDeposit, feeCreate) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractDeposit))) { blockDiffEi =>
//        blockDiffEi shouldBe an[Right[_, _]]
//      }
//    }
//  }
//
//  val newContractWithdraw: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
//  val preconditionsAndExecuteContractWithdraw: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
//    master <- accountGen
//    ts <- positiveIntGen
//    contract1 <- newContractDeposit
//    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
//    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
//    fee <- smallFeeGen
//    feeScale <- feeScaleGen
//    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
//    fee1: Long <- smallFeeGen
//    feeScale1: Short <- feeScaleGen
//    ts1: Long <- positiveLongGen
//    funcIdx1: Short <- Gen.const(FunId.issueIndex)
//    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
//    recipient <- mintingAddressGen
//    fee2: Long <- smallFeeGen
//    feeScale2: Short <- feeScaleGen
//    ts2: Long <- positiveLongGen
//    data2: Seq[DataEntry] <- transferDataStackGen(PublicKeyAccount(master.publicKey).toAddress, recipient, 10000L,0)
//    funcIdx2: Short <- Gen.const(FunId.transferIndex)
//    fee3: Long <- smallFeeGen
//    feeScale3: Short <- feeScaleGen
//    ts3: Long <- positiveLongGen
//    funcIdx3: Short <- Gen.const(FunId.withdrawIndex)
//    data3: Seq[DataEntry] <- withdrawDataStackGen(recipient, PublicKeyAccount(master.publicKey).toAddress, 1000L,0)
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
//    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description, fee1, feeScale1, ts1).right.get
//    executeContractTransfer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description, fee2, feeScale2, ts2).right.get
//    executeContractWithdraw: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx3, data3, description, fee3, feeScale3, ts3).right.get
//  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractWithdraw, executeContractWithdraw.fee)
//
//  property("execute contract transaction withdraw successfully"){
//    forAll(preconditionsAndExecuteContractWithdraw) { case (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractWithdraw, feeCreate) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, executeContractTransfer))), TestBlock.create(Seq(executeContractWithdraw))) { blockDiffEi =>
//        blockDiffEi shouldBe an[Right[_, _]]
//      }
//    }
//  }
//
//  val newContractTotalSupply: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
//  val preconditionsAndExecuteContractTotalSupply: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
//    master <- accountGen
//    ts <- positiveIntGen
//    contract1 <- newContractTotalSupply
//    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
//    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
//    fee <- smallFeeGen
//    feeScale <- feeScaleGen
//    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
//    fee1: Long <- smallFeeGen
//    feeScale1: Short <- feeScaleGen
//    ts1: Long <- positiveLongGen
//    funcIdx1: Short <- Gen.const(FunId.issueIndex)
//    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
//    recipient <- mintingAddressGen
//    fee2: Long <- smallFeeGen
//    feeScale2: Short <- feeScaleGen
//    ts2: Long <- positiveLongGen
//    funcIdx2: Short <- Gen.const(FunId.totalSupplyIndex)
//    data2: Seq[DataEntry] <- totalSupplyDataStackGen(0)
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
//    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description, fee1, feeScale1, ts1).right.get
//    executeContractTotalSupply: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description, fee2, feeScale2, ts2).right.get
//  } yield (genesis, regContract, executeContractIssue, executeContractTotalSupply, executeContractTotalSupply.fee)
//
//  property("execute contract transaction totalSupply successfully"){
//    forAll(preconditionsAndExecuteContractTotalSupply) { case (genesis, regContract, executeContractIssue, executeContractTotalSupply, feeCreate) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractTotalSupply))) { blockDiffEi =>
//        blockDiffEi should produce("Invalid Opc type")
//      }
//    }
//  }
//
//  val newContractMaxSupply: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
//  val preconditionsAndExecuteContractMaxSupply: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
//    master <- accountGen
//    ts <- positiveIntGen
//    contract1 <- newContractMaxSupply
//    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
//    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
//    fee <- smallFeeGen
//    feeScale <- feeScaleGen
//    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
//    fee1: Long <- smallFeeGen
//    feeScale1: Short <- feeScaleGen
//    ts1: Long <- positiveLongGen
//    funcIdx1: Short <- Gen.const(FunId.issueIndex)
//    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
//    recipient <- mintingAddressGen
//    fee2: Long <- smallFeeGen
//    feeScale2: Short <- feeScaleGen
//    ts2: Long <- positiveLongGen
//    funcIdx2: Short <- Gen.const(FunId.maxSupplyIndex)
//    data2: Seq[DataEntry] <- maxSupplyDataStackGen(0)
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
//    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description, fee1, feeScale1, ts1).right.get
//    executeContractMaxSupply: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description, fee2, feeScale2, ts2).right.get
//  } yield (genesis, regContract, executeContractIssue, executeContractMaxSupply, executeContractMaxSupply.fee)
//
//  property("execute contract transaction maxSupply successfully"){
//    forAll(preconditionsAndExecuteContractMaxSupply) { case (genesis, regContract, executeContractIssue, executeContractMaxSupply, feeCreate) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractMaxSupply))) { blockDiffEi =>
//        blockDiffEi should produce("Invalid Opc type")
//      }
//    }
//  }
//
//  val newContractBalanceOf: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
//  val preconditionsAndExecuteContractBalanceOf: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
//    master <- accountGen
//    ts <- positiveIntGen
//    contract1 <- newContractBalanceOf
//    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
//    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
//    fee <- smallFeeGen
//    feeScale <- feeScaleGen
//    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
//    fee1: Long <- smallFeeGen
//    feeScale1: Short <- feeScaleGen
//    ts1: Long <- positiveLongGen
//    funcIdx1: Short <- Gen.const(FunId.issueIndex)
//    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
//    recipient <- mintingAddressGen
//    fee2: Long <- smallFeeGen
//    feeScale2: Short <- feeScaleGen
//    ts2: Long <- positiveLongGen
//    funcIdx2: Short <- Gen.const(FunId.balanceOfIndex)
//    data2: Seq[DataEntry] <- balanceOfDataStackGen(PublicKeyAccount(master.publicKey).toAddress, 0)
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
//    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description, fee1, feeScale1, ts1).right.get
//    executeContractBalanceOf: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, data2, description, fee2, feeScale2, ts2).right.get
//  } yield (genesis, regContract, executeContractIssue, executeContractBalanceOf, executeContractBalanceOf.fee)
//
//  property("execute contract transaction balanceOf successfully"){
//    forAll(preconditionsAndExecuteContractBalanceOf) { case (genesis, regContract, executeContractIssue, executeContractBalanceOf, feeCreate) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractBalanceOf))) { blockDiffEi =>
//        blockDiffEi should produce("Invalid Opc type")
//      }
//    }
//  }
//
//  val newContractGetIssuer: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
//  val preconditionsAndExecuteContractGetIssuer: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
//    master <- accountGen
//    ts <- positiveIntGen
//    contract1 <- newContractGetIssuer
//    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
//    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
//    fee <- smallFeeGen
//    feeScale <- feeScaleGen
//    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
//    fee1: Long <- smallFeeGen
//    feeScale1: Short <- feeScaleGen
//    ts1: Long <- positiveLongGen
//    funcIdx1: Short <- Gen.const(FunId.issueIndex)
//    data1: Seq[DataEntry] <- issueDataStackGen(100000L,0)
//    recipient <- mintingAddressGen
//    fee2: Long <- smallFeeGen
//    feeScale2: Short <- feeScaleGen
//    ts2: Long <- positiveLongGen
//    funcIdx2: Short <- Gen.const(FunId.getIssuerIndex)
//    //data2: Seq[DataEntry] <- ()
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
//    executeContractIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx1, data1, description, fee1, feeScale1, ts1).right.get
//    executeContractGetIssuer: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx2, Nil, description, fee2, feeScale2, ts2).right.get
//  } yield (genesis, regContract, executeContractIssue, executeContractGetIssuer, executeContractGetIssuer.fee)
//
//  property("execute contract transaction getIssuer successfully"){
//    forAll(preconditionsAndExecuteContractGetIssuer) { case (genesis, regContract, executeContractIssue, executeContractGetIssuer, feeCreate) =>
//      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractGetIssuer))) { blockDiffEi =>
//        blockDiffEi should produce("Invalid Opc type")
//      }
//    }
//  }
}
