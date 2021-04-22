package vsys.blockchain.state.contract.token

import vsys.blockchain.state._
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{Contract, ContractGenHelper, DataEntry, DataType}
import vsys.blockchain.contract.lock.LockContractGen
import vsys.blockchain.contract.token.NonFungibleContractV2Gen
import vsys.blockchain.state.ByteStr
import vsys.blockchain.state.diffs.assertDiffAndStateCorrectBlockTime
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

class ExecuteNonFungibleContractV2ValidDiffTest  extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with LockContractGen
  with NonFungibleContractV2Gen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val nonFungibleWhiteContract: Gen[Contract] = nonFungibleContractWhiteGen()
  val nonFungibleBlackContract: Gen[Contract] = nonFungibleContractBlackGen()

  val preconditionsNonFungibleContractV2IssueValidTest: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractWhite <- nonFungibleWhiteContract

    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContract <- registerNonFungibleV2Gen(master, contractWhite, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisNonFungibleV2Gen(master, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractId, issueData, attach, fee, ts+1)
  } yield (genesis, regContract, issue)

  property("Execute issue with non fungible contract V2") {
    forAll(preconditionsNonFungibleContractV2IssueValidTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(reg.timestamp, Seq(reg))),
        TestBlock.createWithTxStatus(issue.timestamp, Seq(issue), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = reg.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val maker = reg.proofs.firstCurveProof.explicitGet().publicKey
        val issuer = issue.proofs.firstCurveProof.explicitGet().publicKey

        //Statevar keys
        val issuerBalanceKey = ByteStr(Bytes.concat(tokenId.arr, issuer.toAddress.bytes.arr))
        val makerBalanceKey = ByteStr(Bytes.concat(tokenId.arr, maker.toAddress.bytes.arr))

        val issuerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        //Statemap keys
        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(issuer.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(maker.toAddress.bytes.arr, DataType.Address))
        newState.tokenAccountBalance(issuerBalanceKey) shouldBe 1L
        newState.tokenAccountBalance(makerBalanceKey) shouldBe 1L
      }
    }
  }

  val preconditionsNonFungibleContractV2UpdateListValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
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

    updateListData = Seq(master.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractId, updateListData, updateListType, attach, fee, ts)

  } yield (genesis, genesis2, regContract, issue, updateList1, updateList2)

  property("Execute update list in non fungible contract V2") {
    forAll(preconditionsNonFungibleContractV2UpdateListValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(reg.timestamp, Seq(reg, issue))),
        TestBlock.createWithTxStatus(updateList1.timestamp, Seq(updateList1), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = reg.contractId.bytes
        val updateLister = updateList1.proofs.firstCurveProof.explicitGet().publicKey
        val updateListKey = ByteStr(Bytes.concat(contractId.arr,  Array(0.toByte), DataEntry(updateLister.toAddress.bytes.arr, DataType.Address).bytes))

        newState.contractInfo(updateListKey) shouldEqual Some(DataEntry(Array(0.toByte), DataType.Boolean))
      }
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(reg.timestamp, Seq(reg, issue, updateList2))),
        TestBlock.createWithTxStatus(updateList1.timestamp, Seq(updateList1), TransactionStatus.Success)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsNonFungibleWhiteContractV2SendValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
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
    forAll(preconditionsNonFungibleWhiteContractV2SendValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractWhite: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction,
    updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, send: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractWhite, issue, updateList3, updateList4, updateList1, updateList2))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regContractWhite.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val master = regContractWhite.proofs.firstCurveProof.explicitGet().publicKey
        val user = genesis2.recipient

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, user.bytes.arr))

        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 1L
      }
    }
  }

  val preconditionsNonFungibleBlackContractV2SendValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
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
    forAll(preconditionsNonFungibleBlackContractV2SendValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractBlack: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction,
    updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, send: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractBlack, issue, updateList3, updateList4, updateList1, updateList2))),
        TestBlock.createWithTxStatus(send.timestamp, Seq(send), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regContractBlack.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val master = regContractBlack.proofs.firstCurveProof.explicitGet().publicKey
        val user = genesis2.recipient

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, user.bytes.arr))

        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 1L
      }
    }
  }

  val preconditionsNonFungibleWhiteContractV2TransferValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
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


    transferData = Seq(master.toAddress.bytes.arr, user.toAddress.bytes.arr, Ints.toByteArray(0))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Int32)
    transfer <- transferNonFungibleV2Gen(master, contractWhiteId, transferData, transferType, attach, fee, ts)


  } yield (genesis, genesis2, regContractWhite, issue, updateList1, updateList2,  updateList3, updateList4, transfer)

  property("Execute transfer in non fungible white contract") {
    forAll(preconditionsNonFungibleWhiteContractV2TransferValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractWhite: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction,
    updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, transfer: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractWhite, issue, updateList3, updateList4, updateList1, updateList2))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regContractWhite.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val master = regContractWhite.proofs.firstCurveProof.explicitGet().publicKey
        val user = genesis2.recipient

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, user.bytes.arr))

        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 1L
      }
    }
  }

  val preconditionsNonFungibleBlackContractV2TransferValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
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

    transferData = Seq(master.toAddress.bytes.arr, user.toAddress.bytes.arr, Ints.toByteArray(0))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Int32)
    transfer <- transferNonFungibleV2Gen(master, contractBlackId, transferData, transferType, attach, fee, ts)


  } yield (genesis, genesis2, regContractBlack, issue, updateList1, updateList2,  updateList3, updateList4, transfer)

  property("Execute transfer in non fungible black contract") {
    forAll(preconditionsNonFungibleWhiteContractV2TransferValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractBlack: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction,
    updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, transfer: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(updateList2.timestamp, Seq(regContractBlack, issue, updateList1, updateList2))),
        TestBlock.createWithTxStatus(transfer.timestamp, Seq(transfer), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regContractBlack.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val master = regContractBlack.proofs.firstCurveProof.explicitGet().publicKey
        val user = genesis2.recipient

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, user.bytes.arr))

        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 1L
      }
    }
  }

  val preconditionsNonFungibleBlackContractV2DepositValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractBlack <- nonFungibleBlackContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContractBlack <- registerNonFungibleV2Gen(master, contractBlack, dataStack, description, fee + 10000000000L, ts)
    contractBlackId = regContractBlack.contractId
    tokenId = tokenIdFromBytes(contractBlackId.bytes.arr, Ints.toByteArray(0)).explicitGet()

    contractLock <- lockContractGen
    dataStack: Seq[DataEntry] <- initLockContractDataStackGen(tokenId.arr)
    description <- validDescStringGen
    regContractLock <- registerLockGen(master, contractLock, dataStack, description, fee + 10000000000L, ts)
    contractLockId = regContractLock.contractId

    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractBlackId, issueData, attach, fee, ts+1)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    depositData = Seq(master.toAddress.bytes.arr, contractLockId.bytes.arr, Ints.toByteArray(0))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Int32)
    deposit <- depositNonFungibleV2Gen(master, contractBlackId, depositData, depositType, attach, fee, ts)

  } yield (genesis, genesis2, regContractBlack, regContractLock, issue, updateList1, deposit)

  property("Execute deposit in non fungible black contract") {
    forAll(preconditionsNonFungibleBlackContractV2DepositValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractBlack: RegisterContractTransaction, regContractLock: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(issue.timestamp, Seq(regContractBlack, regContractLock, issue))),
        TestBlock.createWithTxStatus(deposit.timestamp, Seq(deposit), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regContractBlack.contractId.bytes
        val lockContractId = regContractLock.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val master = regContractBlack.proofs.firstCurveProof.explicitGet().publicKey


        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val contractBalanceKey = ByteStr(Bytes.concat(tokenId.arr, lockContractId.arr))

        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
        newState.tokenAccountBalance(contractBalanceKey) shouldBe 1L
      }
    }
  }

  val preconditionsNonFungibleWhiteContractV2DepositValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractWhite <- nonFungibleWhiteContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContractWhite <- registerNonFungibleV2Gen(master, contractWhite, dataStack, description, fee + 10000000000L, ts)
    contractWhiteId = regContractWhite.contractId
    tokenId = tokenIdFromBytes(contractWhiteId.bytes.arr, Ints.toByteArray(0)).explicitGet()

    contractLock <- lockContractGen
    dataStack: Seq[DataEntry] <- initLockContractDataStackGen(tokenId.arr)
    description <- validDescStringGen
    regContractLock <- registerLockGen(master, contractLock, dataStack, description, fee + 10000000000L, ts)
    contractLockId = regContractLock.contractId

    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractWhiteId, issueData, attach, fee, ts+1)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(contractLockId.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.ContractAccount, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList3 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(contractLockId.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.ContractAccount, DataType.Boolean)
    updateList4 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    depositData = Seq(master.toAddress.bytes.arr, contractLockId.bytes.arr, Ints.toByteArray(0))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Int32)
    deposit <- depositNonFungibleV2Gen(master, contractWhiteId, depositData, depositType, attach, fee, ts)

  } yield (genesis, genesis2, regContractWhite, regContractLock, issue, updateList1, updateList2, updateList3, updateList4, deposit)

  property("Execute deposit in non fungible white contract") {
    forAll(preconditionsNonFungibleWhiteContractV2DepositValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractWhite: RegisterContractTransaction, regContractLock: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction, updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(issue.timestamp, Seq(regContractWhite, regContractLock, issue, updateList1, updateList2))),
        TestBlock.createWithTxStatus(deposit.timestamp, Seq(deposit), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regContractWhite.contractId.bytes
        val lockContractId = regContractLock.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val master = regContractWhite.proofs.firstCurveProof.explicitGet().publicKey

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val contractBalanceKey = ByteStr(Bytes.concat(tokenId.arr, lockContractId.arr))

        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
        newState.tokenAccountBalance(contractBalanceKey) shouldBe 1L
      }
    }
  }

  val preconditionsNonFungibleBlackContractV2WithdrawValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractBlack <- nonFungibleBlackContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContractBlack <- registerNonFungibleV2Gen(master, contractBlack, dataStack, description, fee + 10000000000L, ts)
    contractBlackId = regContractBlack.contractId
    tokenId = tokenIdFromBytes(contractBlackId.bytes.arr, Ints.toByteArray(0)).explicitGet()

    contractLock <- lockContractGen
    dataStack: Seq[DataEntry] <- initLockContractDataStackGen(tokenId.arr)
    description <- validDescStringGen
    regContractLock <- registerLockGen(master, contractLock, dataStack, description, fee + 10000000000L, ts)
    contractLockId = regContractLock.contractId

    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractBlackId, issueData, attach, fee, ts+1)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(contractLockId.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.ContractAccount, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractBlackId, updateListData, updateListType, attach, fee, ts)

    depositData = Seq(master.toAddress.bytes.arr, contractLockId.bytes.arr, Ints.toByteArray(0))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Int32)
    deposit <- depositNonFungibleV2Gen(master, contractBlackId, depositData, depositType, attach, fee, ts)

    withdrawData = Seq(contractLockId.bytes.arr, master.toAddress.bytes.arr, Ints.toByteArray(0))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Int32)
    withdraw <- withdrawNonFungibleV2Gen(master, contractBlackId, withdrawData, withdrawType, attach, fee, ts)

  } yield (genesis, genesis2, regContractBlack, regContractLock, issue, updateList1, updateList2, deposit, withdraw)

  property("Execute withdraw in non fungible black contract") {
    forAll(preconditionsNonFungibleBlackContractV2WithdrawValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractBlack: RegisterContractTransaction, regContractLock: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(issue.timestamp, Seq(regContractBlack, regContractLock, issue, deposit))),
        TestBlock.createWithTxStatus(withdraw.timestamp, Seq(withdraw), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regContractBlack.contractId.bytes
        val lockContractId = regContractLock.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val master = regContractBlack.proofs.firstCurveProof.explicitGet().publicKey

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val contractBalanceKey = ByteStr(Bytes.concat(tokenId.arr, lockContractId.arr))

        newState.tokenAccountBalance(masterBalanceKey) shouldBe 1L
        newState.tokenAccountBalance(contractBalanceKey) shouldBe 0L
      }
    }
  }

  val preconditionsNonFungibleWhiteContractV2WithdrawValidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contractWhite <- nonFungibleWhiteContract
    user <- accountGen
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContractWhite <- registerNonFungibleV2Gen(master, contractWhite, dataStack, description, fee + 10000000000L, ts)
    contractWhiteId = regContractWhite.contractId
    tokenId = tokenIdFromBytes(contractWhiteId.bytes.arr, Ints.toByteArray(0)).explicitGet()

    contractLock <- lockContractGen
    dataStack: Seq[DataEntry] <- initLockContractDataStackGen(tokenId.arr)
    description <- validDescStringGen
    regContractLock <- registerLockGen(master, contractLock, dataStack, description, fee + 10000000000L, ts)
    contractLockId = regContractLock.contractId

    genesis <- genesisNonFungibleV2Gen(master, ts)
    genesis2 <- genesisNonFungibleV2Gen(user, ts)

    issueData = "first token"
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issue <- issueNonFungibleV2Gen(master, contractWhiteId, issueData, attach, fee, ts+1)

    updateListData = Seq(master.toAddress.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList1 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(contractLockId.bytes.arr, Array(1.toByte))
    updateListType = Seq(DataType.ContractAccount, DataType.Boolean)
    updateList2 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(master.toAddress.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.Address, DataType.Boolean)
    updateList3 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    updateListData = Seq(contractLockId.bytes.arr, Array(0.toByte))
    updateListType = Seq(DataType.ContractAccount, DataType.Boolean)
    updateList4 <- updateListNonFungibleV2Gen(master, contractWhiteId, updateListData, updateListType, attach, fee, ts)

    depositData = Seq(master.toAddress.bytes.arr, contractLockId.bytes.arr, Ints.toByteArray(0))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Int32)
    deposit <- depositNonFungibleV2Gen(master, contractWhiteId, depositData, depositType, attach, fee, ts)

    withdrawData = Seq(contractLockId.bytes.arr, master.toAddress.bytes.arr, Ints.toByteArray(0))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Int32)
    withdraw <- withdrawNonFungibleV2Gen(master, contractWhiteId, withdrawData, withdrawType, attach, fee, ts)

  } yield (genesis, genesis2, regContractWhite, regContractLock, issue, updateList1, updateList2, updateList3, updateList4, deposit, withdraw)

  property("Execute withdraw in non fungible white contract") {
    forAll(preconditionsNonFungibleWhiteContractV2WithdrawValidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regContractWhite: RegisterContractTransaction, regContractLock: RegisterContractTransaction,
    issue: ExecuteContractFunctionTransaction, updateList1: ExecuteContractFunctionTransaction, updateList2: ExecuteContractFunctionTransaction, updateList3: ExecuteContractFunctionTransaction, updateList4: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(issue.timestamp, Seq(regContractWhite, regContractLock, issue, updateList1, updateList2, deposit))),
        TestBlock.createWithTxStatus(withdraw.timestamp, Seq(withdraw), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regContractWhite.contractId.bytes
        val lockContractId = regContractLock.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val master = regContractWhite.proofs.firstCurveProof.explicitGet().publicKey

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val contractBalanceKey = ByteStr(Bytes.concat(tokenId.arr, lockContractId.arr))

        newState.tokenAccountBalance(masterBalanceKey) shouldBe 1L
        newState.tokenAccountBalance(contractBalanceKey) shouldBe 0L
      }
    }
  }
}
