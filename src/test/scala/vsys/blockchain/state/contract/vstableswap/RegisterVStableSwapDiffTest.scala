package vsys.blockchain.state.contract.vstableswap

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.vstableswap.VStableSwapContractGen
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.contract._
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract.RegisterContractTransaction

class RegisterVStableSwapDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with VStableSwapContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val stableSwapContract: Gen[Contract] = vStableSwapContractGen()

  val preconditionsAndStableSwapContractTest: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- stableSwapContract
    description <- validDescStringGen
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initVStableSwapDataStackGen(tokenId.arr, tokenId.arr, 5, 1, 1)
    regContract <- registerVStableSwapGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    genesis <- genesisVStableSwapGen(master, ts)
  } yield (genesis, regContract, fee + 10000000000L)

  property("register stable swap contract function transactions doesn't break invariant") {
    forAll(preconditionsAndStableSwapContractTest) { case (genesis, reg: RegisterContractTransaction, fee: Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(reg))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val contractId = reg.contractId.bytes
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val tokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))
        val tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

        val (_, masterTxs) = newState.accountTransactionIds(master, 2, 0)
        masterTxs.size shouldBe 2 // genesis, reg
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractVStableSwap.contract))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(tokenIdKey) shouldEqual Some(DataEntry(tokenId.arr, DataType.TokenId))
      }
    }
  }
}
