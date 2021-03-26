package vsys.blockchain.state.contract.token

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.ContractGenHelper.feeScale
import vsys.blockchain.contract.lock.LockContractGen
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
  with LockContractGen
  with TokenContractV2Gen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val tokenContractWhiteV2: Gen[Contract] = tokenContractV2Gen(true)
  val tokenContractBlackV2: Gen[Contract] = tokenContractV2Gen(false)
  val lockContract: Gen[Contract] = lockContractGen()

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
      contractId, updateListIndex, dataForSupersede, descEx, fee, feeScale, ts + 1000).explicitGet()
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
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- tokenContractWhiteV2
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisTokenGen(master, ts)
    recipient <- mintingAddressGen
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    updateList <- updateListTokenGen(master, contractId, master.toAddress,true, description, fee, ts)
    updateList2 <- updateListTokenGen(master, contractId, recipient,true, description, fee, ts)
    updateList3 <- updateListTokenGen(master, contractId, contractId,true, description, fee, ts)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
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
  } yield (genesis, regContract, executeContractIssue, executeContractTransfer, executeContractDeposit,
    executeContractWithdraw, invalidDeposit, invalidWithdraw, updateList, updateList2, updateList3)

  // self deposit/withdraw/transfer (self contract)
  // no deposit/withdraw trigger function
  property("execute token contract white v2 transaction transfer function to unsupported contract fail"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue,
    executeContractTransfer, _, _, _, _, updateList, _, updateList3) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)),
        TestBlock.create(Seq(regContract, executeContractIssue, updateList, updateList3))),
        TestBlock.createWithTxStatus(Seq(executeContractTransfer), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // no deposit/withdraw trigger function
  property("execute token contract white v2 transaction deposit function to unsupported contract fail"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _,
    executeContractDeposit, _, _, _, updateList, _, updateList3) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)),
        TestBlock.create(Seq(regContract, executeContractIssue, updateList, updateList3))),
        TestBlock.createWithTxStatus(Seq(executeContractDeposit), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // no deposit/withdraw trigger function
  property("execute token contract white v2 transaction withdraw function from unsupported contract fail"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _,
    executeContractWithdraw, _, _, updateList, _, updateList3) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)),
        TestBlock.create(Seq(regContract, executeContractIssue, updateList, updateList3))),
        TestBlock.createWithTxStatus(Seq(executeContractWithdraw), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute token contract white v2 transaction deposit function invalid data type"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _,
    _, invalidDeposit, _, updateList, _, updateList3) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)),
        TestBlock.create(Seq(regContract, executeContractIssue, updateList, updateList3))),
        TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  property("execute token contract white v2 transaction withdraw function invalid data type"){
    forAll(executeTokenContractWhiteTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _,
    _, _, invalidWithdraw, updateList, _, updateList3) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)),
        TestBlock.create(Seq(regContract, executeContractIssue, updateList, updateList3))),
        TestBlock.createWithTxStatus(Seq(invalidWithdraw), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  val preconditionsAndExecuteContractTransactionInvalidWhite: Gen[(GenesisTransaction, GenesisTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    newIssuer <- accountGen
    genesis <- genesisTokenGen(master, ts)
    genesis1 <- genesisTokenGen(newIssuer, ts)
    contract <- tokenContractWhiteV2
    contract2 <- lockContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee, ts)
    blackTokenId = tokenIdFromBytes(regContract.contractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack2 <- initLockContractDataStackGen(blackTokenId.arr)
    regContract2 <- registerTokenGen(master, contract2, dataStack2, description, fee, ts)
    contractId = regContract.contractId
    contractId2 = regContract2.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    invalidSupersede <- supersedeTokenGen(newIssuer, contractId, newIssuer.toAddress, description, fee, ts)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
    invalidIssue <- issueTokenGen(master, contractId, 100000001L, description, fee, ts)
    invalidDestroy <- destroyTokenGen(master, contractId, 100001L, description, fee, ts)
    invalidUpdateList <- updateListTokenGen(newIssuer, contractId, master.toAddress,true, description, fee, ts)
    recipient <- mintingAddressGen
    invalidSend <- sendTokenGen(master, contractId, recipient, 1000000L, description, fee, ts)
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    invalidTransfer <- transferTokenGen(master, contractId, transferData, transferType, description, fee, ts)
    depositData = Seq(master.toAddress.bytes.arr, contractId2.bytes.arr, Longs.toByteArray(0L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    invalidDeposit <- depositTokenGen(master, contractId, depositData, depositType, description, fee, ts)
    withdrawData = Seq(contractId2.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(0L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    invalidWithdraw <- withdrawTokenGen(master, contractId, withdrawData, withdrawType, description, fee, ts)
  } yield (genesis, genesis1, regContract, invalidSupersede, executeContractIssue, invalidIssue,
    invalidDestroy, invalidUpdateList, invalidSend, invalidTransfer, invalidDeposit, invalidWithdraw)

  property("execute contract white v2 transaction invalid supersede function") {
    forAll(preconditionsAndExecuteContractTransactionInvalidWhite) { case (genesis, genesis1, regContract,
    invalidSupersede, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.createWithTxStatus(Seq(invalidSupersede), TransactionStatus.ContractInvalidSigner)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidSigner
      }
    }
  }

  property("execute contract white v2 transaction invalid issue function"){
    forAll(preconditionsAndExecuteContractTransactionInvalidWhite) { case (genesis, _, regContract, _, _,
    invalidIssue, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))),
        TestBlock.createWithTxStatus(Seq(invalidIssue), TransactionStatus.ContractTokenMaxExceeded)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenMaxExceeded
      } // total > max
    }
  }

  property("execute contract white v2 transaction invalid destroy function") {
    forAll(preconditionsAndExecuteContractTransactionInvalidWhite) { case (genesis, _, regContract, _,
    executeContractIssue, _, invalidDestroy, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDestroy), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  property("execute contract white v2 transaction updateList function invalid caller") {
    forAll(preconditionsAndExecuteContractTransactionInvalidWhite) { case (genesis, genesis1, regContract, _, _,
    _, _, invalidUpdateList, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.createWithTxStatus(Seq(invalidUpdateList), TransactionStatus.ContractInvalidCaller)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }
    }
  }

  property("execute contract white v2 transaction send function failed with sender not in white list") {
    forAll(preconditionsAndExecuteContractTransactionInvalidWhite) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, invalidSend, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidSend), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute contract white v2 transaction transfer function failed with sender not in white list") {
    forAll(preconditionsAndExecuteContractTransactionInvalidWhite) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, invalidTransfer, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidTransfer), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute contract white v2 transaction deposit function failed with sender not in white list") {
    forAll(preconditionsAndExecuteContractTransactionInvalidWhite) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, _, invalidDeposit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute contract white v2 transaction withdraw function failed with sender not in white list") {
    forAll(preconditionsAndExecuteContractTransactionInvalidWhite) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, _, _, invalidWithdraw) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidWithdraw), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val executeTokenContractBlackWithInvalidData: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- tokenContractBlackV2
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
      contractId, updateListIndex, dataForSupersede, descEx, fee, feeScale, ts + 1000).explicitGet()
    invalidSend: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, sendIndex, dataForSupersede, descEx, fee, feeScale, ts + 2000).explicitGet()
    invalidSupersede: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master,
      contractId, supersedeIndex, dataForIssueDestorySplit, descEx, fee, feeScale, ts + 3000).explicitGet()
  } yield (genesis, regContract, invalidIssue, invalidUpdateList, invalidSend, invalidSupersede)

  property("execute token contract black v2 transaction fail with invalid data") {
    forAll(executeTokenContractBlackWithInvalidData) { case (genesis, reg, invalid1, invalid2, invalid3, invalid4) =>
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

  val executeTokenContractBlackTransferDepositWithdraw: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- tokenContractBlackV2
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
  property("execute token contract black v2 transaction transfer function to unsupported contract fail"){
    forAll(executeTokenContractBlackTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, executeContractTransfer, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractTransfer), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // no deposit/withdraw trigger function
  property("execute token contract black v2 transaction deposit function to unsupported contract fail"){
    forAll(executeTokenContractBlackTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, executeContractDeposit, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractDeposit), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // no deposit/withdraw trigger function
  property("execute token contract black v2 transaction withdraw function from unsupported contract fail"){
    forAll(executeTokenContractBlackTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, executeContractWithdraw, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractWithdraw), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute token contract black v2 transaction deposit function invalid data type"){
    forAll(executeTokenContractBlackTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, invalidDeposit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  property("execute token contract black v2 transaction withdraw function invalid data type"){
    forAll(executeTokenContractBlackTransferDepositWithdraw) { case (genesis, regContract, executeContractIssue, _, _, _, _, invalidWithdraw) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidWithdraw), TransactionStatus.ContractDataTypeMismatch)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  val preconditionsAndExecuteContractTransactionInvalidBlack: Gen[(GenesisTransaction, GenesisTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    newIssuer <- accountGen
    genesis <- genesisTokenGen(master, ts)
    genesis1 <- genesisTokenGen(newIssuer, ts)
    contract <- tokenContractBlackV2
    contract2 <- lockContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee, ts)
    blackTokenId = tokenIdFromBytes(regContract.contractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack2 <- initLockContractDataStackGen(blackTokenId.arr)
    regContract2 <- registerTokenGen(master, contract2, dataStack2, description, fee, ts)
    contractId = regContract.contractId
    contractId2 = regContract2.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    invalidSupersede <- supersedeTokenGen(newIssuer, contractId, newIssuer.toAddress, description, fee, ts)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
    invalidIssue <- issueTokenGen(master, contractId, 100000001L, description, fee, ts)
    invalidDestroy <- destroyTokenGen(master, contractId, 100001L, description, fee, ts)
    invalidUpdateList <- updateListTokenGen(newIssuer, contractId, master.toAddress,true, description, fee, ts)
    recipient <- mintingAddressGen
    updateList <- updateListTokenGen(master, contractId, master.toAddress,true, description, fee, ts)
    invalidSend <- sendTokenGen(master, contractId, recipient, 1000L, description, fee, ts)
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    invalidTransfer <- transferTokenGen(master, contractId, transferData, transferType, description, fee, ts)
    depositData = Seq(master.toAddress.bytes.arr, contractId2.bytes.arr, Longs.toByteArray(0L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    invalidDeposit <- depositTokenGen(master, contractId, depositData, depositType, description, fee, ts)
    withdrawData = Seq(contractId2.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(0L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    invalidWithdraw <- withdrawTokenGen(master, contractId, withdrawData, withdrawType, description, fee, ts)
  } yield (genesis, genesis1, regContract, invalidSupersede, executeContractIssue, invalidIssue, invalidDestroy,
    invalidUpdateList, invalidSend, updateList, invalidTransfer, invalidDeposit, invalidWithdraw)

  property("execute contract black v2 transaction invalid supersede function") {
    forAll(preconditionsAndExecuteContractTransactionInvalidBlack) { case (genesis, genesis1, regContract,
    invalidSupersede, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))), TestBlock.createWithTxStatus(Seq(invalidSupersede), TransactionStatus.ContractInvalidSigner)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidSigner
      }
    }
  }

  property("execute contract black v2 transaction invalid issue function"){
    forAll(preconditionsAndExecuteContractTransactionInvalidBlack) { case (genesis, _, regContract, _, _,
    invalidIssue, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))),
        TestBlock.createWithTxStatus(Seq(invalidIssue), TransactionStatus.ContractTokenMaxExceeded)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenMaxExceeded
      } // total > max
    }
  }

  property("execute contract black v2 transaction invalid destroy function") {
    forAll(preconditionsAndExecuteContractTransactionInvalidBlack) { case (genesis, _, regContract, _,
    executeContractIssue, _, invalidDestroy, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(invalidDestroy), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  property("execute contract black v2 transaction updateList function invalid caller") {
    forAll(preconditionsAndExecuteContractTransactionInvalidBlack) { case (genesis, genesis1, regContract, _, _, _,
    _, invalidUpdateList, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1, regContract))),
        TestBlock.createWithTxStatus(Seq(invalidUpdateList), TransactionStatus.ContractInvalidCaller)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }
    }
  }

  property("execute contract black v2 transaction send function failed with sender in black list") {
    forAll(preconditionsAndExecuteContractTransactionInvalidBlack) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, invalidSend, updateList, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, updateList))),
        TestBlock.createWithTxStatus(Seq(invalidSend), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute contract black v2 transaction transfer function failed with sender in black list") {
    forAll(preconditionsAndExecuteContractTransactionInvalidBlack) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, updateList, invalidTransfer, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, updateList))),
        TestBlock.createWithTxStatus(Seq(invalidTransfer), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute contract black v2 transaction deposit function failed with sender in black list") {
    forAll(preconditionsAndExecuteContractTransactionInvalidBlack) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, updateList, _, invalidDeposit, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, updateList))),
        TestBlock.createWithTxStatus(Seq(invalidDeposit), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("execute contract black v2 transaction withdraw function failed with sender in black list") {
    forAll(preconditionsAndExecuteContractTransactionInvalidBlack) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, updateList, _, _, invalidWithdraw) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, updateList))),
        TestBlock.createWithTxStatus(Seq(invalidWithdraw), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

}
