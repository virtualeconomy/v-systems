package vsys.blockchain.state.contract.token

import com.google.common.primitives.Longs
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.ContractGenHelper.feeScale
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract._

class ExecuteTokenContractInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val tokenContract: Gen[Contract] = tokenContractGen(true)

  val executeTokenContractWithInvalidData: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- tokenContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenContractGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisTokenGen(master, ts)
    rep <- mintingAddressGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    dataForIssueDestorySplit: Seq[DataEntry] <- amountDataStackGen(10000L)
    dataForSend: Seq[DataEntry] <- sendDataStackGen(rep, 100L)
    dataForSupersede: Seq[DataEntry] <- addressDataStackGen(rep)
    invalidIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, issueIndex, dataForSend, descEx, fee, feeScale, ts + 1000).explicitGet()
    invalidSplit: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, splitIndex, dataForSupersede, descEx, fee, feeScale, ts + 1000).explicitGet()
    invalidSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, sendIndex, dataForSupersede, descEx, fee, feeScale, ts + 2000).explicitGet()
    invalidSupersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, supersedeIndex, dataForIssueDestorySplit, descEx, fee, feeScale, ts + 3000).explicitGet()
  } yield (genesis, regContract, invalidIssue, invalidSplit, invalidSend, invalidSupersede)

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

  val executeTokenContractTransferDepositWithdraw: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- tokenContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenContractGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisTokenGen(master, ts)
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
    recipient <- mintingAddressGen
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    transferData2 = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(1000L))
    transferType2 = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    executeContractTransfer <- transferTokenGen(master, contractId, true, transferData2, transferType2, description, fee, ts)
    executeContractDeposit <- depositTokenGen(master, contractId, true, transferData2, transferType2, description, fee, ts)
    withdrawData = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(0L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    executeContractWithdraw <- withdrawTokenGen(master, contractId, true, withdrawData, withdrawType, description, fee, ts)
    invalidDeposit <- depositTokenGen(master, contractId, true, transferData, transferType, description, fee, ts)
    invalidWithdrawData = Seq(recipient.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(1000L))
    invalidWithdrawType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    invalidWithdraw <- withdrawTokenGen(master, contractId, true, invalidWithdrawData, invalidWithdrawType, description, fee, ts)
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractDeposit, executeContractWithdraw, invalidDeposit, invalidWithdraw)

  // self deposit/withdraw/transfer (self contract)
  // no deposit/withdraw trigger function
  property("execute token contract transaction transfer function to unsupported contract fail"){
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
  property("execute token contract transaction deposit function to unsupported contract fail"){
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
  property("execute token contract transaction withdraw function from unsupported contract fail"){
    forAll(executeTokenContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, executeContractWithdraw, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractWithdraw), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }



  property("execute token contract transaction deposit function invalid data type"){
    forAll(executeTokenContractTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, invalidDeposit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  property("execute token contract transaction withdraw function invalid data type"){
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
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    newIssuer <- accountGen
    genesis <- genesisTokenGen(master, ts)
    genesis1 <- genesisTokenGen(newIssuer, ts)
    contract <- tokenContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenContractGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    invalidSupersede <- supersedeTokenGen(newIssuer, contractId, newIssuer.toAddress, description, fee, ts)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
    invalidIssue <- issueTokenGen(master, contractId, 100000001L, description, fee, ts)
    invalidDestroy <- destroyTokenGen(master, contractId, 100001L, description, fee, ts)
    invalidSplit <- splitTokenGen(master, contractId, 0L, description, fee, ts)
    recipient <- mintingAddressGen
    invalidSend <- sendTokenGen(master, contractId, true, recipient, 1000000L, description, fee, ts)
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
