package vsys.blockchain.state.contract.token

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.utils.serialization.Deser
import vsys.account.Address
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract._
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract._

class ExecuteTokenContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with TokenContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val tokenContract: Gen[Contract] = tokenContractGen(true)

  val preconditionsAndExecuteContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    user <- accountGen
    contract <- tokenContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisTokenGen(master, ts)
    genesis2 <- genesisTokenGen(user, ts)
    feeEx: Long <- smallFeeGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    split <- splitTokenGen(master, contractId, 1000L, descEx, feeEx, ts + 1)
    supersede <- supersedeTokenGen(master, contractId, user.toAddress, descEx, feeEx, ts + 2)
    issue <- issueTokenGen(user, contractId, 10000L, descEx, feeEx, ts + 3)
    destroy <- destroyTokenGen(user, contractId, 100L, descEx, feeEx, ts + 4)
    send <- sendTokenGen(user, contractId, true, master.toAddress, 500L, descEx, feeEx, ts + 5)
    selfSend <- sendTokenGen(user, contractId, true, user.toAddress, 500L, descEx, feeEx, ts + 6)
  } yield (genesis, genesis2, regContract, split, supersede, issue, destroy, send, selfSend, send.transactionFee)

  property("execute token contract function transactions doesn't break invariant") {
    forAll(preconditionsAndExecuteContractTest) { case (genesis, genesis2, reg: RegisterContractTransaction, split, supersede,
                                                        issue, destroy, send: ExecuteContractFunctionTransaction, selfSend, feeEx: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg, split, supersede, issue, destroy))), TestBlock.create(Seq(send, selfSend))) { (blockDiff, newState) =>
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

        val (_, masterTxs) = newState.accountTransactionIds(master, 5, 0)
        masterTxs.size shouldBe 5 // genesis, reg, split, supersede, send
        val (_, userTxs) = newState.accountTransactionIds(user, 6, 0)
        userTxs.size shouldBe 6 // genesis2, supersede, issue, destory, send, selfSend

        newState.contractTokens(contractId) shouldBe 1
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractPermitted.contract))

        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(user.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.tokenInfo(maxKey) shouldEqual Some(DataEntry(Longs.toByteArray(100000000L), DataType.Amount))
        newState.tokenAccountBalance(totalKey) shouldBe 9900L
        newState.tokenInfo(unityKey) shouldEqual Some(DataEntry(Longs.toByteArray(1000L), DataType.Amount))
        newState.tokenInfo(descKey) shouldEqual Some(DataEntry.create(descDE, DataType.ShortText).explicitGet())
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 500L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 9400L
      }
    }
  }

  val preconditionsAndExecuteContractTransaction: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    newIssuer <- accountGen
    genesis <- genesisTokenGen(master, ts)
    genesis1 <- genesisTokenGen(newIssuer, ts)
    contract <- tokenContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    regContract <- registerTokenGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractSupersede <- supersedeTokenGen(master, contractId, newIssuer.toAddress, description, fee, ts)
    executeContractIssue <- issueTokenGen(master, contractId, 100000L, description, fee, ts)
    executeContractIssue1 <- issueTokenGen(newIssuer, contractId, 100000L, description, fee, ts)
    executeContractDestroy <- destroyTokenGen(master, contractId, 10000L, description, fee, ts)
    executeContractSplit <- splitTokenGen(master, contractId, 10000L, description, fee, ts)
    recipient <- mintingAddressGen
    executeContractSend <- sendTokenGen(master, contractId, true, recipient, 100000L, description, fee, ts)
    executeContractSelfSend <- sendTokenGen(master, contractId, true, master.toAddress, 100000L, description, fee, ts)
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Amount)
    executeContractTransfer <- transferTokenGen(master, contractId, true, transferData, transferType, description, fee, ts)
    executeContractTotalSupply <- totalSupplyTokenGen(master, contractId, true, description, fee, ts)
    executeContractMaxSupply <- maxSupplyTokenGen(master, contractId, true, description, fee, ts)
    executeContractBalanceOf <- balanceOfTokenGen(master, contractId, true, master.toAddress, description, fee, ts)
    executeContractGetIssuer <- getIssuerTokenGen(master, contractId, true, description, fee, ts)
  } yield (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue, executeContractIssue1,
    executeContractDestroy, executeContractSplit, executeContractSend, executeContractSelfSend, executeContractTransfer,
    executeContractTotalSupply, executeContractMaxSupply, executeContractBalanceOf, executeContractGetIssuer, executeContractSelfSend.transactionFee)

  property("execute token contract transaction supersede function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue,
    executeContractIssue1, _, _, _, _, _, _, _, _, _, _) =>
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

  property("execute token contract transaction issue function successfully"){
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _,
    executeContractIssue, _, _, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractIssue))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract transaction destroy function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _,
    executeContractDestroy, _, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractDestroy))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract transaction split function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, _, _, _,
    executeContractSplit, _, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractSplit))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract transaction send function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _,
    executeContractSend, _, _, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractSend))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract transaction send function self send successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract: RegisterContractTransaction, _, executeContractIssue, _, _, _, _,
    executeContractSelfSend: ExecuteContractFunctionTransaction, _, _, _, _, _, feeSelfSend: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractSelfSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = executeContractSelfSend.proofs.firstCurveProof.explicitGet().publicKey
        totalPortfolioDiff.balance shouldBe -feeSelfSend
        totalPortfolioDiff.effectiveBalance shouldBe -feeSelfSend
        val contractId = regContract.contractId.bytes
        val tokenId = tokenIdFromBytes(contractId.arr, Ints.toByteArray(0)).explicitGet()
        val senderBalanceKey = ByteStr(Bytes.concat(tokenId.arr, sender.toAddress.bytes.arr))
        val (_, senderTxs) = newState.accountTransactionIds(sender.toAddress, 4, 0)
        senderTxs.size shouldBe 4 // genesis and payment, issue and send
        newState.tokenAccountBalance(senderBalanceKey) shouldBe 100000L
      }
    }
  }

  property("execute token contract transaction transfer function to address successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _,
    _, _, executeContractTransfer, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.create(Seq(executeContractTransfer))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute token contract transaction totalSupply function unsupported") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, executeContractTotalSupply, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractTotalSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract transaction maxSupply function unsupported") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, executeContractMaxSupply, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractMaxSupply), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract transaction balanceOf function unsupported") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, _, executeContractBalanceOf, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractBalanceOf), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  property("execute token contract transaction getIssuer function unsupported") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue,
    _, _, _, _, _, _, _, _, _, executeContractGetIssuer, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))),
        TestBlock.createWithTxStatus(Seq(executeContractGetIssuer), TransactionStatus.ContractUnsupportedOPC)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractUnsupportedOPC
      }
    }
  }

  val preconditionsAndExecuteContractSystemSend: Gen[(GenesisTransaction, ExecuteContractFunctionTransaction, Long, Address)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisTokenGen(master, ts)
    recipient <- mintingAddressGen
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractSystemSend <- sendVSYSGen(master, recipient, 100000L, description, fee, ts)
  } yield (genesis, executeContractSystemSend, fee, recipient)

  property("execute contract transaction systemSend successfully") {
    forAll(preconditionsAndExecuteContractSystemSend) { case (genesis, executeContractSystemSend: ExecuteContractFunctionTransaction, fee: Long, recipient) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(executeContractSystemSend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = executeContractSystemSend.proofs.firstCurveProof.toOption.get.publicKey
        val recipientPortfolio = newState.accountPortfolio(recipient)
        val senderPortfolio = newState.accountPortfolio(sender)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        recipientPortfolio shouldBe Portfolio(100000, LeaseInfo.empty, Map.empty)
        senderPortfolio shouldBe Portfolio(ENOUGH_AMT - 100000L - fee, LeaseInfo.empty, Map.empty)
        val (_, senderTxs) = newState.accountTransactionIds(sender.toAddress, 2, 0)
        senderTxs.size shouldBe 2 // genesis and send
      }
    }
  }

}
