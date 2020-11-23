package vsys.blockchain.state.contract.vswap

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{Contract, ContractGenHelper, ContractVSwap, DataEntry, DataType}
import vsys.blockchain.contract.vswap.VSwapContractGen
import vsys.blockchain.state.diffs.assertDiffAndState
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.transaction.contract.RegisterContractTransaction

class RegisterVSwapContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with VSwapContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val languageCode: String = "vdds"
  val languageVersion: Int = 1

  val preconditionAndBuildVSwapContract: Gen[(Array[Byte], Array[Byte], Seq[Array[Byte]],
    Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- ContractGenHelper.languageCodeGen(languageCode)
    langVer <- ContractGenHelper.languageVersionGen(languageVersion)
    init <- Gen.const(ContractVSwap.contract.trigger)
    descriptor <- Gen.const(ContractVSwap.contract.descriptor)
    stateVar <- Gen.const(ContractVSwap.contract.stateVar)
    stateMap <- Gen.const(ContractVSwap.contract.stateMap)
    textual <- Gen.const(ContractVSwap.contract.textual)
  } yield (langCode, langVer, init, descriptor, stateVar, stateMap, textual)

  property("register v-swap contract build doesn't break invariant"){
    forAll(preconditionAndBuildVSwapContract) { case (langCode, langVer, init, descriptor, stateVar, stateMap, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, stateMap, textual) shouldBe an[Right[_, _]]
    }
  }

  val validContract: Gen[Contract] = vSwapContractGen()
  val preconditionsAndRegContractTest: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisVSwapGen(master, ts)
    contract <- validContract
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data: Seq[DataEntry] <- initVSwapDataStackGen(tokenId.arr, tokenId.arr, tokenId.arr, 1)
    description <- validDescStringGen
    create <- registerVSwapGen(master, contract, data, description, fee, ts + 1)
  } yield (genesis, create)

  property("register v-swap contract transaction doesn't break invariant") {
    forAll(preconditionsAndRegContractTest) { case (genesis, reg: RegisterContractTransaction) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(reg))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -reg.transactionFee
        totalPortfolioDiff.effectiveBalance shouldBe -reg.transactionFee
        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val contractId = reg.contractId.bytes
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))

        val (_, masterTxs) = newState.accountTransactionIds(master, 2, 0)
        masterTxs.size shouldBe 2 // genesis, reg
        newState.contractTokens(contractId) shouldBe 0
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractVSwap.contract))

        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
      }
    }
  }
}
