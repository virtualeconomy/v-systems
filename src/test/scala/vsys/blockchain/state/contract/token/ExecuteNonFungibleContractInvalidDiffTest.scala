package vsys.blockchain.state.contract.token

import com.google.common.primitives.Ints
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.ContractGenHelper.feeScale
import vsys.blockchain.contract.token.NonFungibleContractGen
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract._

class ExecuteNonFungibleContractInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with NonFungibleContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val nonFungibleContract: Gen[Contract] = nonFungibleContractGen()

  val executeTokenContractWithInvalidData: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- nonFungibleContractGen()
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContract <- registerNonFungibleGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisNonFungibleGen(master, ts)
    rep <- mintingAddressGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    dataForIssue: Seq[DataEntry] <- issueDataStackGen("first token")
    dataForSend: Seq[DataEntry] <- sendDataStackGen(rep, 1)
    dataForSupersede: Seq[DataEntry] <- addressDataStackGen(rep)
    invalidIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, issueIndex, dataForSend, descEx, fee, feeScale, ts + 1000).explicitGet()
    invalidSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, sendIndex, dataForSupersede, descEx, fee, feeScale, ts + 2000).explicitGet()
    invalidSend1: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, sendIndex, dataForSend, descEx, fee, feeScale, ts + 1000).explicitGet()
    invalidSupersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, supersedeIndex, dataForIssue, descEx, fee, feeScale, ts + 3000).explicitGet()
  } yield (genesis, regContract, invalidIssue, invalidSend, invalidSend1, invalidSupersede)

  property("execute contract transaction fail with invalid data"){
    forAll(executeTokenContractWithInvalidData) { case (genesis, reg, invalid1, invalid2, invalid3, invalid4) =>
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

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid3), TransactionStatus.ContractInvalidTokenIndex)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidTokenIndex
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, reg))), TestBlock.createWithTxStatus(Seq(invalid4), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }

    }
  }

  val executeTokenContractTransferDepositWithdraw: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- nonFungibleContractGen()
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContract <- registerNonFungibleGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisNonFungibleGen(master, ts)
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractIssue <- issueNonFungibleGen(master, contractId, "first token", description, fee, ts)
    recipient <- mintingAddressGen
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Ints.toByteArray(0))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Int32)
    transferData2 = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Ints.toByteArray(0))
    transferType2 = Seq(DataType.Address, DataType.ContractAccount, DataType.Int32)
    executeContractTransfer <- transferNonFungibleGen(master, contractId, transferData2, transferType2, description, fee, ts)
    executeContractDeposit <- depositNonFungibleGen(master, contractId, transferData2, transferType2, description, fee, ts)
    withdrawData = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Ints.toByteArray(0))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Int32)
    executeContractWithdraw <- withdrawNonFungibleGen(master, contractId, withdrawData, withdrawType, description, fee, ts)
    invalidDeposit <- depositNonFungibleGen(master, contractId, transferData, transferType, description, fee, ts)
    invalidWithdrawData = Seq(recipient.bytes.arr, master.toAddress.bytes.arr, Ints.toByteArray(0))
    invalidWithdrawType = Seq(DataType.Address, DataType.Address, DataType.Int32)
    invalidWithdraw <- withdrawNonFungibleGen(master, contractId, invalidWithdrawData, invalidWithdrawType, description, fee, ts)
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractDeposit, executeContractWithdraw, invalidDeposit, invalidWithdraw)

  // self deposit/withdraw/transfer (self contract)
  // no deposit/withdraw trigger function
  property("execute non-fungible contract transaction transfer function to unsupported contract fail"){
    forAll(executeTokenContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, executeContractTransfer, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractTransfer), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // no deposit/withdraw trigger function
  property("execute non-fungible contract transaction deposit function to unsupported contract fail"){
    forAll(executeTokenContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, executeContractDeposit, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractDeposit), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // no deposit/withdraw trigger function
  property("execute non-fungible contract transaction withdraw function from unsupported contract fail"){
    forAll(executeTokenContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, executeContractWithdraw, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractWithdraw), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }



  property("execute non-fungible contract transaction deposit function invalid data type"){
    forAll(executeTokenContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, invalidDeposit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  property("execute non-fungible contract transaction withdraw function invalid data type"){
    forAll(executeTokenContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, _, invalidWithdraw) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidWithdraw), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  val preconditionsAndExecuteContractTransactionInvalid: Gen[(GenesisTransaction, GenesisTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    newIssuer <- accountGen
    genesis <- genesisNonFungibleGen(master, ts)
    genesis1 <- genesisNonFungibleGen(newIssuer, ts)
    contract <- nonFungibleContractGen()
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContract <- registerNonFungibleGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    invalidSupersede <- supersedeNonFungibleGen(newIssuer, contractId, newIssuer.toAddress, description, fee, ts)
    executeContractIssue <- issueNonFungibleGen(master, contractId, "first token", description, fee, ts)
    recipient <- mintingAddressGen
    send <- sendNonFungibleGen(master, contractId, recipient, 0, description, fee, ts)
    invalidSend <- sendNonFungibleGen(master, contractId, recipient, 0, description, fee, ts + 1)
  } yield (genesis, genesis1, regContract, invalidSupersede, executeContractIssue, send, invalidSend)

  property("execute contract transaction invalid supersede function") {
    forAll(preconditionsAndExecuteContractTransactionInvalid) { case (genesis, genesis1, regContract, invalidSupersede, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.createWithTxStatus(Seq(invalidSupersede), TransactionStatus.ContractInvalidSigner)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidSigner
      }
    }
  }

  property("execute contract transaction send function failed with insufficient token balance") {
    forAll(preconditionsAndExecuteContractTransactionInvalid) { case (genesis, _, regContract, _, executeContractIssue, send, invalidSend) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, send))),
        TestBlock.createWithTxStatus(Seq(invalidSend), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

}
