package vsys.blockchain.state.contract.token

import com.google.common.primitives.Longs
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.ContractGenHelper.feeScale
import vsys.blockchain.contract.{Contract, ContractGenHelper, DataEntry, DataType}
import vsys.blockchain.contract.token.TokenContractV2Gen
import vsys.blockchain.state.EitherExt2
import vsys.blockchain.state.diffs.assertDiffEi
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

class ExecuteTokenContractV2InvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractV2Gen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val tokenContractWhiteV2: Gen[Contract] = tokenContractV2Gen(true)

  val executeTokenContractWhiteWithInvalidData: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- tokenContractWhiteV2
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisTokenGen(master, ts)
    rep <- mintingAddressGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    dataForIssueDestorySplit: Seq[DataEntry] <- amountDataStackGen(10000L)
    dataForSend: Seq[DataEntry] <- sendDataStackGen(rep, 100L)
    dataForSupersede: Seq[DataEntry] <- addressDataStackGen(rep)
    invalidIssue: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, issueIndex, dataForSend, descEx, fee, feeScale, ts + 1000).explicitGet()
    invalidUpdateList: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, splitIndex, dataForSupersede, descEx, fee, feeScale, ts + 1000).explicitGet()
    invalidSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, sendIndex, dataForSupersede, descEx, fee, feeScale, ts + 2000).explicitGet()
    invalidSupersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, supersedeIndex, dataForIssueDestorySplit, descEx, fee, feeScale, ts + 3000).explicitGet()
  } yield (genesis, regContract, invalidIssue, invalidUpdateList, invalidSend, invalidSupersede)

  property("execute token contract white v2 transaction fail with invalid data") {
    forAll(executeTokenContractWhiteWithInvalidData) { case (genesis, reg, invalid1, invalid2, invalid3, invalid4) =>
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

  val executeTokenContractWhiteTransferDepositWithdraw: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- tokenContractWhiteV2
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisTokenGen(master, ts)
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
    recipient <- mintingAddressGen
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    transferData2 = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(1000L))
    transferType2 = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    executeContractTransfer <- transferTokenGen(master, contractId, transferData2, transferType2, description, fee, ts)
    executeContractDeposit <- depositTokenGen(master, contractId, transferData2, transferType2, description, fee, ts)
    withdrawData = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(0L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    executeContractWithdraw <- withdrawTokenGen(master, contractId, withdrawData, withdrawType, description, fee, ts)
    invalidDeposit <- depositTokenGen(master, contractId, transferData, transferType, description, fee, ts)
    invalidWithdrawData = Seq(recipient.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(1000L))
    invalidWithdrawType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    invalidWithdraw <- withdrawTokenGen(master, contractId, invalidWithdrawData, invalidWithdrawType, description, fee, ts)
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractDeposit, executeContractWithdraw, invalidDeposit, invalidWithdraw)

  // self deposit/withdraw/transfer (self contract)
  // no deposit/withdraw trigger function
  property("execute token contract white v2 transaction transfer function to unsupported contract fail"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, executeContractTransfer, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractTransfer), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // no deposit/withdraw trigger function
  property("execute token contract white v2 transaction deposit function to unsupported contract fail"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, executeContractDeposit, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractDeposit), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // no deposit/withdraw trigger function
  property("execute token contract white v2 transaction withdraw function from unsupported contract fail"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, executeContractWithdraw, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractWithdraw), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }



  property("execute token contract white v2 transaction deposit function invalid data type"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, invalidDeposit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  property("execute token contract white v2 transaction withdraw function invalid data type"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, _, invalidWithdraw) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidWithdraw), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

}
