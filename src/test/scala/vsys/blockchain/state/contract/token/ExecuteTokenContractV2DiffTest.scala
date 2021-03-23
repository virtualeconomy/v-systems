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
import vsys.blockchain.state.diffs.assertDiffAndState
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
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

  val tokenContractV2: Gen[Contract] = tokenContractV2Gen(true)

  val preconditionsAndExecuteContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    user <- accountGen
    contract <- tokenContractV2
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
    supersede <- supersedeTokenGen(master, contractId, user.toAddress, descEx, feeEx, ts + 3)
    issue <- issueTokenGen(user, contractId, 10000L, descEx, feeEx, ts + 4)
    destroy <- destroyTokenGen(user, contractId, 100L, descEx, feeEx, ts + 5)
    send <- sendTokenGen(user, contractId, master.toAddress, 500L, descEx, feeEx, ts + 6)
    selfSend <- sendTokenGen(user, contractId, user.toAddress, 500L, descEx, feeEx, ts + 7)
  } yield (genesis, genesis2, regContract, updateList, updateList2, supersede, issue, destroy, send, selfSend, send.transactionFee)

  property("execute token contract v2 function transactions doesn't break invariant") {
    forAll(preconditionsAndExecuteContractTest) { case (genesis, genesis2, reg: RegisterContractTransaction, updateList, updateList2, supersede,
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

        val (_, masterTxs) = newState.accountTransactionIds(master, 5, 0)
        masterTxs.size shouldBe 5 // genesis, reg, split, supersede, send
        val (_, userTxs) = newState.accountTransactionIds(user, 6, 0)
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
}
