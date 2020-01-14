package vsys.blockchain.state.contract.token

import cats.Monoid
import com.google.common.primitives.{Bytes}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.token.NonFungibleContractGen
import vsys.blockchain.state.{ByteStr, EitherExt2, Portfolio}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.transaction.contract._

class RegisterNonFungibleContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with NonFungibleContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val languageCode: String = "vdds"
  val languageVersion: Int = 1

  val preconditionAndBuildRegContract: Gen[(Array[Byte], Array[Byte], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- ContractGenHelper.languageCodeGen(languageCode)
    langVer <- ContractGenHelper.languageVersionGen(languageVersion)
    init <- Gen.const(ContractNonFungible.contract.trigger)
    descriptor <- Gen.const(ContractNonFungible.contract.descriptor)
    stateVar <- Gen.const(ContractNonFungible.contract.stateVar)
    textual <- Gen.const(ContractNonFungible.contract.textual)
  } yield (langCode, langVer, init, descriptor, stateVar, textual)

  property("register non-fungible contract build doesn't break invariant"){
    forAll(preconditionAndBuildRegContract) { case (langCode, langVer, init, descriptor, stateVar, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, Seq(), textual) shouldBe an[Right[_, _]]
    }
  }

  val validContract: Gen[Contract] = nonFungibleContractGen()
  val preconditionsAndRegContractTest: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisNonFungibleGen(master, ts)
    contract <- validContract
    data: Seq[DataEntry] <- initTokenDataStackGen()
    description <- validDescStringGen
    create <- registerNonFungibleGen(master, contract, data, description, fee, ts + 1)
  } yield (genesis, create)

  property("register non-fungible contract transaction doesn't break invariant") {
    forAll(preconditionsAndRegContractTest) { case (genesis, reg: RegisterContractTransaction) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(reg))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -reg.transactionFee
        totalPortfolioDiff.effectiveBalance shouldBe -reg.transactionFee
        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val contractId = reg.contractId.bytes
        val issuerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        val (_, masterTxs) = newState.accountTransactionIds(master, 2, 0)
        masterTxs.size shouldBe 2 // genesis, reg
        newState.contractTokens(contractId) shouldBe 0
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractNonFungible.contract))

        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
      }
    }
  }

}
