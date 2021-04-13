package vsys.blockchain.state.contract.token

import org.scalacheck.{Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{Contract, ContractGenHelper, DataEntry, DataType}
import vsys.blockchain.contract.token.NonFungibleContractV2Gen
import vsys.blockchain.state.diffs.assertDiffAndStateCorrectBlockTime
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

class ExecuteNonFungibleContractV2InvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with NonFungibleContractV2Gen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val nonFungibleWhiteContract: Gen[Contract] = nonFungibleContractWhiteGen()
  val nonFungibleBlackContract: Gen[Contract] = nonFungibleContractBlackGen()

  val preconditionsNonFungibleContractV2UpdateListInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractWhite <- nonFungibleWhiteContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContract <- registerNonFungibleV2Gen(master, contractWhite, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractId, issueData, attach, fee, ts+1)

    updateListData = Seq(user.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(user.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(user.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList3 <- updateListNonFungibleV2Gen(user, contractId, updateListData, updateListType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, issue, updateList1, updateList2, updateList3)

  property("Execute update list in non fungible contract V2") {
    forAll(preconditionsNonFungibleContractV2UpdateListInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction, updateList3: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(reg.timestamp, Seq(reg, issue))),
        TestBlock.createWithTxStatus(updateList1.timestamp, Seq(updateList1), TransactionStatus.Success)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(reg.timestamp, Seq(reg, issue))),
        TestBlock.createWithTxStatus(updateList2.timestamp, Seq(updateList2), TransactionStatus.Success)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(reg.timestamp, Seq(reg, issue))),
        TestBlock.createWithTxStatus(updateList3.timestamp, Seq(updateList3), TransactionStatus.ContractInvalidCaller)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }
    }
  }

  val preconditionsNonFungibleWhiteContractV2SendInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractWhite <- nonFungibleWhiteContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContractWhite <- registerNonFungibleV2Gen(master, contractWhite, dataStack, description, fee + 10000000000L, ts)
    contractWhiteId = regContractWhite.contractId

    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractWhiteId, issueData, attach, fee, ts+1)

    updateListData = Seq(user.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(user.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList3 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList4 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    send <- sendNonFungibleV2Gen(master, contractWhiteId, user, 0, attach, fee, ts)


  } yield (genesis, genesis2, regContractWhite, issue, updateList1, updateList2,  updateList3, updateList4, send)

  property("Execute send in non fungible white contract") {
    forAll(preconditionsNonFungibleWhiteContractV2SendInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractWhite: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction,
    updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, send: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractWhite, issue, updateList1, updateList2))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Success)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      // only receiver on white list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList1.timestamp, Seq(regContractWhite, issue, updateList1))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // only sender on white list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractWhite, issue, updateList2))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // update receiver not on white list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList3.timestamp, Seq(regContractWhite, issue, updateList1, updateList2, updateList3))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // update sender not on white list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList4.timestamp, Seq(regContractWhite, issue, updateList1, updateList2, updateList4))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsNonFungibleBlackContractV2SendInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractBlack <- nonFungibleBlackContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContractBlack <- registerNonFungibleV2Gen(master, contractBlack, dataStack, description, fee + 10000000000L, ts)
    contractBlackId = regContractBlack.contractId

    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractBlackId, issueData, attach, fee, ts+1)

    updateListData = Seq(user.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(user.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList3 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList4 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    send <- sendNonFungibleV2Gen(master, contractBlackId, user, 0, attach, fee, ts)


  } yield (genesis, genesis2, regContractBlack, issue, updateList1, updateList2,  updateList3, updateList4, send)

  property("Execute send in non fungible black contract") {
    forAll(preconditionsNonFungibleBlackContractV2SendInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractBlack: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction,
    updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, send: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractBlack, issue, updateList1, updateList2))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Success)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      // only receiver on black list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList1.timestamp, Seq(regContractBlack, issue, updateList3))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // only sender on black list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractBlack, issue, updateList4))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // update receiver not on black list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList3.timestamp, Seq(regContractBlack, issue, updateList3, updateList4, updateList1))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // update sender not on black list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList4.timestamp, Seq(regContractBlack, issue, updateList3, updateList4, updateList2))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsNonFungibleWhiteContractV2TransferInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractWhite <- nonFungibleWhiteContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContractWhite <- registerNonFungibleV2Gen(master, contractWhite, dataStack, description, fee + 10000000000L, ts)
    contractWhiteId = regContractWhite.contractId

    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractWhiteId, issueData, attach, fee, ts+1)

    updateListData = Seq(user.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(user.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList3 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList4 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)


    transferData = Seq(master.toAddress.bytes.arr, user.toAddress.bytes.arr, Array(0.toByte))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Int32)
    transfer <- transferNonFungibleV2Gen(master, contractWhiteId, transferData, transferType, attach, fee, ts)


  } yield (genesis, genesis2, regContractWhite, issue, updateList1, updateList2,  updateList3, updateList4, transfer)

  property("Execute transfer in non fungible white contract") {
    forAll(preconditionsNonFungibleWhiteContractV2SendInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractWhite: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction,
    updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, transfer: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractWhite, issue, updateList1, updateList2))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Success)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      // only receiver on white list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList1.timestamp, Seq(regContractWhite, issue, updateList1))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // only sender on white list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractWhite, issue, updateList2))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // update receiver not on white list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList3.timestamp, Seq(regContractWhite, issue, updateList1, updateList2, updateList3))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // update sender not on white list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList4.timestamp, Seq(regContractWhite, issue, updateList1, updateList2, updateList4))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }





  val preconditionsNonFungibleBlackContractV2TransferInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractBlack <- nonFungibleBlackContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContractBlack <- registerNonFungibleV2Gen(master, contractBlack, dataStack, description, fee + 10000000000L, ts)
    contractBlackId = regContractBlack.contractId

    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractBlackId, issueData, attach, fee, ts+1)

    updateListData = Seq(user.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(user.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList3 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList4 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    transferData = Seq(master.toAddress.bytes.arr, user.toAddress.bytes.arr, Array(0.toByte))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Int32)
    transfer <- transferNonFungibleV2Gen(master, contractBlackId, transferData, transferType, attach, fee, ts)


  } yield (genesis, genesis2, regContractBlack, issue, updateList1, updateList2,  updateList3, updateList4, transfer)

  property("Execute transfer in non fungible black contract") {
    forAll(preconditionsNonFungibleBlackContractV2SendInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractBlack: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction,
    updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, transfer: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractBlack, issue, updateList1, updateList2))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Success)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      // only receiver on black list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList1.timestamp, Seq(regContractBlack, issue, updateList3))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // only sender on black list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractBlack, issue, updateList4))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // update receiver not on black list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList3.timestamp, Seq(regContractBlack, issue, updateList3, updateList4, updateList1))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // update sender not on black list
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList4.timestamp, Seq(regContractBlack, issue, updateList3, updateList4, updateList2))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}
