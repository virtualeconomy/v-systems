package vsys.blockchain.state.contract.token

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.utils.serialization.Deser
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract._
import vsys.blockchain.contract.basic.BasicContractGen
import vsys.blockchain.contract.token.{NonFungibleContractGen, SystemContractGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract._

class ExecuteNonFungibleContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with BasicContractGen
  with NonFungibleContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val nonFungibleContract: Gen[Contract] = nonFungibleContractGen()
  val simpleDepositWithdrawContract: Gen[Contract] = contractGen(false)

  val preconditionsAndExecuteContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    user <- accountGen
    contract <- nonFungibleContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    regContract <- registerNonFungibleGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    genesis <- genesisNonFungibleGen(master, ts)
    genesis2 <- genesisNonFungibleGen(user, ts)
    feeEx: Long <- smallFeeGen
    descEx <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    supersede <- supersedeNonFungibleGen(master, contractId, user.toAddress, descEx, feeEx, ts + 2)
    issue <- issueNonFungibleGen(user, contractId, "first token", descEx, feeEx, ts + 3)
    issue1 <- issueNonFungibleGen(user, contractId, "second token", descEx, feeEx, ts + 4)
    send <- sendNonFungibleGen(user, contractId, master.toAddress, 0, descEx, feeEx, ts + 5)
    selfSend <- sendNonFungibleGen(master, contractId, master.toAddress, 0, descEx, feeEx, ts + 6)
  } yield (genesis, genesis2, regContract, supersede, issue, issue1, send, selfSend, send.transactionFee)

  property("execute non-fungible contract function transactions doesn't break invariant") {
    forAll(preconditionsAndExecuteContractTest) { case (genesis, genesis2, reg: RegisterContractTransaction, supersede,
    issue, issue1, send: ExecuteContractFunctionTransaction, selfSend, feeEx: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg, supersede, issue, issue1))), TestBlock.create(Seq(send, selfSend))) { (blockDiff, newState) =>
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
        val descDE = Deser.serilizeString("first token")
        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, user.toAddress.bytes.arr))
        val tokenId1 = tokenIdFromBytes(contractId.arr, Ints.toByteArray(1)).explicitGet()
        val issuerKey1 = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val makerKey1 = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))
        val maxKey1 = ByteStr(Bytes.concat(tokenId1.arr, Array(0.toByte)))
        val totalKey1 = ByteStr(Bytes.concat(tokenId1.arr, Array(1.toByte)))
        val unityKey1 = ByteStr(Bytes.concat(tokenId1.arr, Array(2.toByte)))
        val descKey1 = ByteStr(Bytes.concat(tokenId1.arr, Array(3.toByte)))
        val descDE1 = Deser.serilizeString("second token")
        val masterBalanceKey1 = ByteStr(Bytes.concat(tokenId1.arr, master.toAddress.bytes.arr))
        val userBalanceKey1 = ByteStr(Bytes.concat(tokenId1.arr, user.toAddress.bytes.arr))

        val (_, masterTxs) = newState.accountTransactionIds(master, 5, 0)
        masterTxs.size shouldBe 5 // genesis, reg, supersede, send, selfsend
        val (_, userTxs) = newState.accountTransactionIds(user, 4, 0)
        userTxs.size shouldBe 4 // genesis2, supersede, issue, send

        newState.contractTokens(contractId) shouldBe 2
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractNonFungible.contract))

        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(user.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.tokenInfo(maxKey) shouldEqual Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.tokenAccountBalance(totalKey) shouldBe 1L
        newState.tokenInfo(unityKey) shouldEqual Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.tokenInfo(descKey) shouldEqual Some(DataEntry.create(descDE, DataType.ShortText).explicitGet())
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 1L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 0L

        newState.contractInfo(issuerKey1) shouldEqual Some(DataEntry(user.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey1) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.tokenInfo(maxKey1) shouldEqual Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.tokenAccountBalance(totalKey1) shouldBe 1L
        newState.tokenInfo(unityKey1) shouldEqual Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.tokenInfo(descKey1) shouldEqual Some(DataEntry.create(descDE1, DataType.ShortText).explicitGet())
        newState.tokenAccountBalance(masterBalanceKey1) shouldBe 0L
        newState.tokenAccountBalance(userBalanceKey1) shouldBe 1L
      }
    }
  }

  val preconditionsAndExecuteContractTransaction: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    Long)] = for {
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
    executeContractSupersede <- supersedeNonFungibleGen(master, contractId, newIssuer.toAddress, description, fee, ts)
    executeContractIssue <- issueNonFungibleGen(master, contractId, "first token", description, fee, ts)
    executeContractIssue1 <- issueNonFungibleGen(newIssuer, contractId, "first token", description, fee, ts)
    recipient <- mintingAddressGen
    executeContractSend <- sendNonFungibleGen(master, contractId, recipient, 0, description, fee, ts)
    executeContractSelfSend <- sendNonFungibleGen(master, contractId, master.toAddress, 0, description, fee, ts)
    transferData = Seq(master.toAddress.bytes.arr, recipient.bytes.arr, Ints.toByteArray(0))
    transferType = Seq(DataType.Address, DataType.Address, DataType.Int32)
    executeContractTransfer <- transferNonFungibleGen(master, contractId, transferData, transferType, description, fee, ts)
  } yield (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue, executeContractIssue1,
    executeContractSend, executeContractSelfSend, executeContractTransfer, executeContractSelfSend.transactionFee)

  property("execute non-fungible contract transaction supersede function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, genesis1, regContract, executeContractSupersede, executeContractIssue,
    executeContractIssue1, _, _, _, _) =>
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

  property("execute non-fungible contract transaction issue function successfully"){
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract))), TestBlock.create(Seq(executeContractIssue))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute non-fungible contract transaction send function successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _, executeContractSend, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, regContract, executeContractIssue))), TestBlock.create(Seq(executeContractSend))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute non-fungible contract transaction send function self send successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract: RegisterContractTransaction, _, executeContractIssue, _, _,
    executeContractSelfSend: ExecuteContractFunctionTransaction, _, feeSelfSend: Long) =>
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
        newState.tokenAccountBalance(senderBalanceKey) shouldBe 1L
      }
    }
  }

  property("execute non-fungible contract transaction transfer function to address successfully") {
    forAll(preconditionsAndExecuteContractTransaction) { case (genesis, _, regContract, _, executeContractIssue, _,
    _, _, executeContractTransfer, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract, executeContractIssue))),
        TestBlock.create(Seq(executeContractTransfer))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.tokenAccountBalance.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndDepositWithdrawContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisBasicGen(master, ts)
    user <- accountGen
    genesis2 <- genesisBasicGen(user, ts)
    contract <- simpleDepositWithdrawContract
    description <- validDescStringGen
    regContract <- registerDepositWithdrawContractGen(master, contract, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    nonFungibleContract <- nonFungibleContractGen()
    dataStack: Seq[DataEntry] <- initTokenDataStackGen()
    registerNonFungibleContract <- registerNonFungibleGen(master, nonFungibleContract, dataStack, description, fee + 10000000000L, ts)
    nonFungibleContractId = registerNonFungibleContract.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractIssue <- issueNonFungibleGen(master, nonFungibleContractId, "first token", description, fee, ts)
    transferData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Ints.toByteArray(0))
    transferType = Seq(DataType.Address, DataType.ContractAccount, DataType.Int32)
    executeContractDeposit <- depositNonFungibleGen(master, nonFungibleContractId, transferData, transferType, description, fee, ts + 1)
    withdrawData = Seq(contractId.bytes.arr, user.toAddress.bytes.arr, Ints.toByteArray(0))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Int32)
    executeContractWithdraw <- withdrawNonFungibleGen(user, nonFungibleContractId, withdrawData, withdrawType, description, fee, ts + 2)
    depositData = Seq(user.toAddress.bytes.arr, contractId.bytes.arr, Ints.toByteArray(0))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Int32)
    executeContractDeposit2 <- depositNonFungibleGen(user, nonFungibleContractId, depositData, depositType, description, fee, ts + 4)
  } yield (genesis, genesis2, regContract, registerNonFungibleContract, executeContractIssue, executeContractDeposit,
    executeContractWithdraw, executeContractDeposit2, fee)

  property("execute deposit withdraw interact with non-fungible contract functions doesn't break invariant") {
    forAll(preconditionsAndDepositWithdrawContractTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction,
    reg: RegisterContractTransaction, regToken: RegisterContractTransaction, issue, deposit,
    withdraw: ExecuteContractFunctionTransaction, deposit2, fee: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg, regToken, issue))),
        TestBlock.create(Seq(deposit, withdraw, deposit2))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee * 3
        totalPortfolioDiff.effectiveBalance shouldBe -fee * 3
        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val user = withdraw.proofs.firstCurveProof.explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val contractId = reg.contractId.bytes
        val tokenContractId = regToken.contractId.bytes
        val tokenId = tokenIdFromBytes(tokenContractId.arr, Ints.toByteArray(0)).explicitGet()

        val issuerKey = ByteStr(Bytes.concat(tokenContractId.arr, Array(0.toByte)))
        val makerKey = ByteStr(Bytes.concat(tokenContractId.arr, Array(1.toByte)))
        val maxKey = ByteStr(Bytes.concat(tokenId.arr, Array(0.toByte)))
        val totalKey = ByteStr(Bytes.concat(tokenId.arr, Array(1.toByte)))
        val unityKey = ByteStr(Bytes.concat(tokenId.arr, Array(2.toByte)))
        val descKey = ByteStr(Bytes.concat(tokenId.arr, Array(3.toByte)))
        val descDE = Deser.serilizeString("first token")

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, masterBytes))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, userBytes))
        val contractBalanceKey = ByteStr(Bytes.concat(tokenId.arr, contractId.arr))

        val (_, masterTxs) = newState.accountTransactionIds(master, 10, 0)
        masterTxs.size shouldBe 5 // genesis, reg, regToken, issue, deposit
        val (_, userTxs) = newState.accountTransactionIds(user, 10, 0)
        userTxs.size shouldBe 3 //genesis2, withdraw, deposit2
        // deposit withdraw contract
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractDepositWithdraw.contract))
        // token contract
        newState.contractTokens(tokenContractId) shouldBe 1
        newState.contractContent(tokenContractId) shouldEqual Some((2, regToken.id, ContractNonFungible.contract))
        // token contract token Info
        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.tokenInfo(maxKey) shouldEqual Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.tokenAccountBalance(totalKey) shouldBe 1L
        newState.tokenInfo(unityKey) shouldEqual Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.tokenInfo(descKey) shouldEqual Some(DataEntry.create(descDE, DataType.ShortText).explicitGet())
        // token balance
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
        newState.tokenAccountBalance(contractBalanceKey) shouldBe 1L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 0L
      }
    }
  }
  
}
