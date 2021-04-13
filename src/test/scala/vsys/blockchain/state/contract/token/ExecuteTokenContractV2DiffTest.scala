package vsys.blockchain.state.contract.token

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{Contract, ContractGenHelper, ContractTokenV2, DataEntry, DataType}
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractV2Gen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs.{assertDiffAndState, assertDiffEi}
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.utils.serialization.Deser

class ExecuteTokenContractV2DiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with TokenContractV2Gen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val tokenContractWhiteV2: Gen[Contract] = tokenContractV2Gen(true)
  val tokenContractBlackV2: Gen[Contract] = tokenContractV2Gen(false)

  val preconditionsAndExecuteContractTestWhite: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    user <- accountGen
    contract <- tokenContractWhiteV2
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisTokenGen(master, ts)
    genesis2 <- genesisTokenGen(user, ts)
    feeEx: Long <- smallFeeGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    updateList <- updateListTokenGen(master, contractId, master.toAddress, true, descEx, feeEx, ts + 1)
    updateList2 <- updateListTokenGen(master, contractId, user.toAddress, true, descEx, feeEx, ts + 2)
    supersede <- supersedeTokenGen(master, contractId, user.toAddress, user.toAddress, descEx, feeEx, ts + 3)
    issue <- issueTokenGen(user, contractId, 10000L, descEx, feeEx, ts + 4)
    destroy <- destroyTokenGen(user, contractId, 100L, descEx, feeEx, ts + 5)
    send <- sendTokenGen(user, contractId, master.toAddress, 500L, descEx, feeEx, ts + 6)
    selfSend <- sendTokenGen(user, contractId, user.toAddress, 500L, descEx, feeEx, ts + 7)
  } yield (genesis, genesis2, regContract, updateList, updateList2, supersede, issue, destroy, send, selfSend, send.transactionFee)

  property("execute token contract white v2 function transactions doesn't break invariant") {
    forAll(preconditionsAndExecuteContractTestWhite) { case (genesis, genesis2, reg: RegisterContractTransaction, updateList, updateList2, supersede,
    issue, destroy, send: ExecuteContractFunctionTransaction, selfSend, feeEx: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg, updateList, updateList2, supersede, issue, destroy))), TestBlock.create(Seq(send, selfSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -(feeEx + feeEx)
        totalPortfolioDiff.effectiveBalance shouldBe -(feeEx + feeEx)
        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val user = send.proofs.firstCurveProof.explicitGet().publicKey
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
        val masterUpdateListKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes))
        val userUpdateListKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes))

        val (_, masterTxs) = newState.accountTransactionIds(master, 10, 0)
        masterTxs.size shouldBe 6 // genesis, reg, update, update2, supersede, send
        val (_, userTxs) = newState.accountTransactionIds(user, 10, 0)
        userTxs.size shouldBe 6 // genesis2, supersede, issue, destory, send, selfSend

        // contract tokens
        newState.contractTokens(contractId) shouldBe 1

        // contract info
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractTokenV2.contractTokenWhiteList))
        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(user.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(masterUpdateListKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(userUpdateListKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))

        // token info
        newState.tokenInfo(maxKey) shouldEqual Some(DataEntry(Longs.toByteArray(100000000L), DataType.Amount))
        newState.tokenInfo(unityKey) shouldEqual Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.tokenInfo(descKey) shouldEqual Some(DataEntry.create(descDE, DataType.ShortText).explicitGet())
        newState.tokenAccountBalance(totalKey) shouldBe 10000L - 100L //destroy 100

        // token account balance
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 500L // user send 500 to master
        newState.tokenAccountBalance(userBalanceKey) shouldBe 10000L - 100L - 500L // send out 500 destroy 100
      }
    }
  }

  val preconditionsAndExecuteContractTestBlack: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    user <- accountGen
    contract <- tokenContractBlackV2
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisTokenGen(master, ts)
    genesis2 <- genesisTokenGen(user, ts)
    feeEx: Long <- smallFeeGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    updateList <- updateListTokenGen(master, contractId, master.toAddress, false, descEx, feeEx, ts + 1)
    updateList2 <- updateListTokenGen(master, contractId, user.toAddress, false, descEx, feeEx, ts + 2)
    supersede <- supersedeTokenGen(master, contractId, user.toAddress, user.toAddress, descEx, feeEx, ts + 3)
    issue <- issueTokenGen(user, contractId, 10000L, descEx, feeEx, ts + 4)
    destroy <- destroyTokenGen(user, contractId, 100L, descEx, feeEx, ts + 5)
    send <- sendTokenGen(user, contractId, master.toAddress, 500L, descEx, feeEx, ts + 6)
    selfSend <- sendTokenGen(user, contractId, user.toAddress, 500L, descEx, feeEx, ts + 7)
  } yield (genesis, genesis2, regContract, updateList, updateList2, supersede, issue, destroy, send, selfSend, send.transactionFee)

  property("execute token contract black v2 function transactions doesn't break invariant") {
    forAll(preconditionsAndExecuteContractTestBlack) { case (genesis, genesis2, reg: RegisterContractTransaction, updateList, updateList2, supersede,
    issue, destroy, send: ExecuteContractFunctionTransaction, selfSend, feeEx: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg, updateList, updateList2, supersede, issue, destroy))), TestBlock.create(Seq(send, selfSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -(feeEx + feeEx)
        totalPortfolioDiff.effectiveBalance shouldBe -(feeEx + feeEx)
        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val user = send.proofs.firstCurveProof.explicitGet().publicKey
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
        val masterUpdateListKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes))
        val userUpdateListKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes))

        val (_, masterTxs) = newState.accountTransactionIds(master, 10, 0)
        masterTxs.size shouldBe 6 // genesis, reg, update, update2, supersede, send
        val (_, userTxs) = newState.accountTransactionIds(user, 10, 0)
        userTxs.size shouldBe 6 // genesis2, supersede, issue, destory, send, selfSend

        // contract tokens
        newState.contractTokens(contractId) shouldBe 1

        // contract info
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractTokenV2.contractTokenBlackList))
        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(user.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(masterUpdateListKey) shouldEqual Some(DataEntry(Array(0.toByte), DataType.Boolean))
        newState.contractInfo(userUpdateListKey) shouldEqual Some(DataEntry(Array(0.toByte), DataType.Boolean))

        // token info
        newState.tokenInfo(maxKey) shouldEqual Some(DataEntry(Longs.toByteArray(100000000L), DataType.Amount))
        newState.tokenInfo(unityKey) shouldEqual Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.tokenInfo(descKey) shouldEqual Some(DataEntry.create(descDE, DataType.ShortText).explicitGet())
        newState.tokenAccountBalance(totalKey) shouldBe 10000L - 100L //destroy 100

        // token account balance
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 500L // user send 500 to master
        newState.tokenAccountBalance(userBalanceKey) shouldBe 10000L - 100L - 500L // send out 500 destroy 100
      }
    }
  }

  val preconditionsAndExecuteContractTransactionWhite: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    newIssuer <- accountGen
    genesis <- genesisTokenGen(master, ts)
    genesis1 <- genesisTokenGen(newIssuer, ts)
    contract <- tokenContractWhiteV2
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractSupersede <- supersedeTokenGen(master, contractId, newIssuer.toAddress, newIssuer.toAddress, description, fee, ts)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
    executeContractIssue1 <- issueTokenGen(newIssuer, contractId, 100000L, description, fee, ts)
    executeContractDestroy <- destroyTokenGen(master, contractId, 10000L, description, fee, ts)
    executeContractUpdateList <- updateListTokenGen(master, contractId, master.toAddress,true, description, fee, ts)
    recipient <- mintingAddressGen
    executeContractUpdateList2 <- updateListTokenGen(master, contractId, recipient,true, description, fee, ts)
    executeContractSend <- sendTokenGen(master, contractId, recipient, 100000L, description, fee, ts)
    executeContractSelfSend <- sendTokenGen(master, contractId, master.toAddress, 100000L, description, fee, ts)
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    executeContractTransfer <- transferTokenGen(master, contractId, transferData, transferType, description, fee, ts)
    executeContractTotalSupply <- totalSupplyTokenGen(master, contractId, description, fee, ts)
    executeContractMaxSupply <- maxSupplyTokenGen(master, contractId, description, fee, ts)
    executeContractBalanceOf <- balanceOfTokenGen(master, contractId, master.toAddress, description, fee, ts)
    executeContractGetIssuer <- getIssuerTokenGen(master, contractId, description, fee, ts)
  } yield (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue, executeContractIssue1,
    executeContractDestroy, executeContractUpdateList, executeContractUpdateList2, executeContractSend,
    executeContractSelfSend, executeContractTransfer, executeContractTotalSupply, executeContractMaxSupply,
    executeContractBalanceOf, executeContractGetIssuer, executeContractSelfSend.transactionFee)

  property("execute token contract white v2 transaction supersede function successfully") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue,
    executeContractIssue1, _, _, _, _, _, _, _, _, _, _, _) =>
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

  property("execute token contract white v2 transaction issue function successfully"){
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractIssue))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract white v2 transaction destroy function successfully") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _, executeContractIssue, _,
    executeContractDestroy, _, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractDestroy))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract white v2 transaction updateList function successfully") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _, _, _, _,
    executeContractUpdateList, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractUpdateList))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract white v2 transaction send function successfully") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _, executeContractIssue, _, _, executeContractUpdateList, executeContractUpdateList2,
    executeContractSend, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, executeContractUpdateList, executeContractUpdateList2))),
        TestBlock.create(Seq(executeContractSend))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract white v2 transaction send function self send successfully") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract: RegisterContractTransaction, _, executeContractIssue, _, _, executeContractUpdateList, _, _,
    executeContractSelfSend: ExecuteContractFunctionTransaction, _, _, _, _, _, feeSelfSend: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, executeContractUpdateList))),
        TestBlock.create(Seq(executeContractSelfSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = executeContractSelfSend.proofs.firstCurveProof.explicitGet().publicKey
        totalPortfolioDiff.balance shouldBe -feeSelfSend
        totalPortfolioDiff.effectiveBalance shouldBe -feeSelfSend
        val contractId = regContract.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val senderBalanceKey = ByteStr(Bytes.concat(tokenId.arr, sender.toAddress.bytes.arr))
        val (_, senderTxs) = newState.accountTransactionIds(sender.toAddress, 10, 0)
        senderTxs.size shouldBe 5 // genesis and payment, issue, updateList and send
        newState.tokenAccountBalance(senderBalanceKey) shouldBe 100000L
      }
    }
  }

  property("execute token contract white v2 transaction transfer function to address successfully") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _, executeContractIssue, _, _, executeContractUpdateList,
    executeContractUpdateList2, _, _, executeContractTransfer, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue, executeContractUpdateList, executeContractUpdateList2))),
        TestBlock.create(Seq(executeContractTransfer))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract white v2 transaction totalSupply function unsupported") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, executeContractTotalSupply, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractTotalSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract white v2 transaction maxSupply function unsupported") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, _, executeContractMaxSupply, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractMaxSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract white v2 transaction balanceOf function unsupported") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, _, _, executeContractBalanceOf, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractBalanceOf), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract white v2 transaction getIssuer function unsupported") {
    forAll(preconditionsAndExecuteContractTransactionWhite) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, _, _, _, executeContractGetIssuer, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractGetIssuer), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  val preconditionsAndExecuteContractTransactionBlack: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    newIssuer <- accountGen
    genesis <- genesisTokenGen(master, ts)
    genesis1 <- genesisTokenGen(newIssuer, ts)
    contract <- tokenContractBlackV2
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractSupersede <- supersedeTokenGen(master, contractId, newIssuer.toAddress, newIssuer.toAddress, description, fee, ts)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
    executeContractIssue1 <- issueTokenGen(newIssuer, contractId, 100000L, description, fee, ts)
    executeContractDestroy <- destroyTokenGen(master, contractId, 10000L, description, fee, ts)
    executeContractUpdateList <- updateListTokenGen(master, contractId, master.toAddress,false, description, fee, ts)
    recipient <- mintingAddressGen
    executeContractUpdateList2 <- updateListTokenGen(master, contractId, recipient,false, description, fee, ts)
    executeContractSend <- sendTokenGen(master, contractId, recipient, 100000L, description, fee, ts)
    executeContractSelfSend <- sendTokenGen(master, contractId, master.toAddress, 100000L, description, fee, ts)
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    executeContractTransfer <- transferTokenGen(master, contractId, transferData, transferType, description, fee, ts)
    executeContractTotalSupply <- totalSupplyTokenGen(master, contractId, description, fee, ts)
    executeContractMaxSupply <- maxSupplyTokenGen(master, contractId, description, fee, ts)
    executeContractBalanceOf <- balanceOfTokenGen(master, contractId, master.toAddress, description, fee, ts)
    executeContractGetIssuer <- getIssuerTokenGen(master, contractId, description, fee, ts)
  } yield (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue, executeContractIssue1,
    executeContractDestroy, executeContractUpdateList, executeContractUpdateList2, executeContractSend,
    executeContractSelfSend, executeContractTransfer, executeContractTotalSupply, executeContractMaxSupply,
    executeContractBalanceOf, executeContractGetIssuer, executeContractSelfSend.transactionFee)

  property("execute token contract black v2 transaction supersede function successfully") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue,
    executeContractIssue1, _, _, _, _, _, _, _, _, _, _, _) =>
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

  property("execute token contract black v2 transaction issue function successfully"){
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractIssue))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract black v2 transaction destroy function successfully") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _, executeContractIssue, _,
    executeContractDestroy, _, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractDestroy))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract black v2 transaction updateList function successfully") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _, _, _, _,
    executeContractUpdateList, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractUpdateList))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract black v2 transaction send function successfully") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _, executeContractIssue, _, _, executeContractUpdateList, executeContractUpdateList2,
    executeContractSend, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, executeContractUpdateList, executeContractUpdateList2))),
        TestBlock.create(Seq(executeContractSend))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract black v2 transaction send function self send successfully") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract: RegisterContractTransaction, _, executeContractIssue, _, _, executeContractUpdateList, _, _,
    executeContractSelfSend: ExecuteContractFunctionTransaction, _, _, _, _, _, feeSelfSend: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue, executeContractUpdateList))),
        TestBlock.create(Seq(executeContractSelfSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = executeContractSelfSend.proofs.firstCurveProof.explicitGet().publicKey
        totalPortfolioDiff.balance shouldBe -feeSelfSend
        totalPortfolioDiff.effectiveBalance shouldBe -feeSelfSend
        val contractId = regContract.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val senderBalanceKey = ByteStr(Bytes.concat(tokenId.arr, sender.toAddress.bytes.arr))
        val (_, senderTxs) = newState.accountTransactionIds(sender.toAddress, 10, 0)
        senderTxs.size shouldBe 5 // genesis and payment, issue, updateList and send
        newState.tokenAccountBalance(senderBalanceKey) shouldBe 100000L
      }
    }
  }

  property("execute token contract black v2 transaction transfer function to address successfully") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _, executeContractIssue, _, _, executeContractUpdateList,
    executeContractUpdateList2, _, _, executeContractTransfer, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue, executeContractUpdateList, executeContractUpdateList2))),
        TestBlock.create(Seq(executeContractTransfer))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract black v2 transaction totalSupply function unsupported") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, executeContractTotalSupply, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractTotalSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract black v2 transaction maxSupply function unsupported") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, _, executeContractMaxSupply, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractMaxSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract black v2 transaction balanceOf function unsupported") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, _, _, executeContractBalanceOf, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractBalanceOf), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract black v2 transaction getIssuer function unsupported") {
    forAll(preconditionsAndExecuteContractTransactionBlack) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, _, _, _, executeContractGetIssuer, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractGetIssuer), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

}
