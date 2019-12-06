package vsys.blockchain.state.contract.basic

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.blockchain.block.TestBlock
import vsys.utils.serialization.Deser
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract._
import vsys.blockchain.contract.basic.BasicContractGen
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.blockchain.transaction.contract._

class ExecuteDepositWithdrawContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with BasicContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val simpleDepositWithdrawContract: Gen[Contract] = contractGen(false)
  val tokenContract: Gen[Contract] = tokenContractGen(false)

  val preconditionsAndDepositWithdrawContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisBasicGen(master, ts)
    user <- accountGen
    genesis2 <- genesisBasicGen(user, ts)
    contract <- simpleDepositWithdrawContract
    description <- validDescStringGen
    regContract <- registerDepositWithdrawContractGen(master, contract, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    tokenContract <- tokenContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    regTokenContract <- registerTokenContractGen(master, tokenContract, dataStack, description, fee + 10000000000L, ts)
    tokenContractId = regTokenContract.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    executeContractIssue <- issueTokenGen(master, tokenContractId, 100000L, description, fee, ts)
    transferData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    executeContractTransfer <- transferTokenGen(master, tokenContractId, false, transferData, transferType, description, fee, ts)
    executeContractDeposit <- depositTokenGen(master, tokenContractId, false, transferData, transferType, description, fee, ts + 1)
    withdrawData = Seq(contractId.bytes.arr, user.toAddress.bytes.arr, Longs.toByteArray(100L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    executeContractWithdraw <- withdrawTokenGen(user, tokenContractId, false, withdrawData, withdrawType, description, fee, ts + 2)
    withdrawData2 = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(100L))
    withdrawType2 = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    executeContractWithdraw2 <- withdrawTokenGen(master, tokenContractId, false, withdrawData2, withdrawType2, description, fee, ts + 3)
    depositData = Seq(user.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    executeContractDeposit2 <- depositTokenGen(user, tokenContractId, false, depositData, depositType, description, fee, ts + 4)
  } yield (genesis, genesis2, regContract, regTokenContract, executeContractIssue, executeContractTransfer, executeContractDeposit,
    executeContractWithdraw, executeContractWithdraw2, executeContractDeposit2, fee)

  property("execute deposit withdraw interact with token contract functions doesn't break invariant") {
    forAll(preconditionsAndDepositWithdrawContractTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction,
    reg: RegisterContractTransaction, regToken: RegisterContractTransaction, issue, transfer, deposit,
    withdraw: ExecuteContractFunctionTransaction, withdraw2, deposit2, fee: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg, regToken, issue))),
        TestBlock.create(Seq(transfer, deposit, withdraw, withdraw2, deposit2))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee * 5
        totalPortfolioDiff.effectiveBalance shouldBe -fee * 5
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val user = EllipticCurve25519Proof.fromBytes(withdraw.proofs.proofs.head.bytes.arr).explicitGet().publicKey
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
        val descDE = Deser.serilizeString("init")

        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, masterBytes))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, userBytes))
        val contractBalanceKey = ByteStr(Bytes.concat(tokenId.arr, contractId.arr))

        newState.accountTransactionIds(master, 10, 0)._2.size shouldBe 7 // genesis, reg, regToken, issue, transfer, deposit, withdraw2
        newState.accountTransactionIds(user, 10, 0)._2.size shouldBe 3 //genesis2, withdraw, deposit2
        // deposit withdraw contract
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractDepositWithdraw.contract))
        // token contract
        newState.contractTokens(tokenContractId) shouldBe 1
        newState.contractContent(tokenContractId) shouldEqual Some((2, regToken.id, ContractPermitted.contractWithoutSplit))
        // token contract token Info
        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.tokenInfo(maxKey) shouldEqual Some(DataEntry(Longs.toByteArray(100000000L), DataType.Amount))
        newState.tokenAccountBalance(totalKey) shouldBe 100000L
        newState.tokenInfo(unityKey) shouldEqual Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.tokenInfo(descKey) shouldEqual Some(DataEntry.create(descDE, DataType.ShortText).explicitGet())
        // token balance
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 98100L
        newState.tokenAccountBalance(contractBalanceKey) shouldBe 1810L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 90L
      }
    }
  }

  val depositWithdrawContract: Gen[Contract] = contractGen(true)
  val preconditionsSystemContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisBasicGen(master, ts)
    user <- accountGen
    genesis2 <- genesisBasicGen(user, ts)
    contract <- depositWithdrawContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initDepositWithdrawDataStackGen(sysTokenId.arr)
    regContract <- registerDepositWithdrawProductiveContractGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    description <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)

    transferData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(1000L))
    transferType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    executeContractTransfer <- transferVSYSGen(master, transferData, transferType, description, fee, ts)
    executeContractDeposit <- depositVSYSGen(master, transferData, transferType, description, fee, ts + 1)

    withdrawData = Seq(contractId.bytes.arr, user.toAddress.bytes.arr, Longs.toByteArray(100L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    executeContractWithdraw <- withdrawVSYSGen(user, withdrawData, withdrawType, description, fee, ts + 2)

    withdrawData2 = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(100L))
    withdrawType2 = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    executeContractWithdraw2 <- withdrawVSYSGen(master, withdrawData2, withdrawType2, description, fee, ts + 3)

    depositData = Seq(user.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    executeContractDeposit2 <- depositVSYSGen(user, depositData, depositType, description, fee, ts + 4)
  } yield (genesis, genesis2, regContract, executeContractTransfer, executeContractDeposit,
    executeContractWithdraw, executeContractWithdraw2, executeContractDeposit2, fee)

  property("execute deposit withdraw contract function transactions doesn't break invariant") {
    forAll(preconditionsSystemContractTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction,
    reg: RegisterContractTransaction, transfer, deposit, withdraw: ExecuteContractFunctionTransaction,
    withdraw2, deposit2, fee: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2)), TestBlock.create(Seq(reg))),
        TestBlock.create(Seq(transfer, deposit, withdraw, withdraw2, deposit2))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee * 5
        totalPortfolioDiff.effectiveBalance shouldBe -fee * 5

        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val user = EllipticCurve25519Proof.fromBytes(withdraw.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val masterAddress = genesis.recipient
        val userAddress= genesis2.recipient
        val contractId = reg.contractId.bytes
        val contractAddress = reg.contractId

        newState.accountTransactionIds(master, 8, 0)._2.size shouldBe 5 // genesis, reg, transfer, deposit, withdraw2
        newState.accountTransactionIds(user, 8, 0)._2.size shouldBe 3 //genesis2, withdraw, deposit2
        // deposit withdraw contract
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractDepositWithdrawProductive.contract))
        // V balance
        newState.balance(masterAddress) shouldBe ENOUGH_AMT - 4 * fee - 10000000000L - 2 * 1000L + 100L
        newState.balance(userAddress) shouldBe ENOUGH_AMT - 2 * fee + 100L - 10L
        newState.balance(contractAddress) shouldBe 2000L - 200L + 10L
      }
    }
  }

}
