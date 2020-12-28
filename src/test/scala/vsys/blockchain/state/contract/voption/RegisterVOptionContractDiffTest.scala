package vsys.blockchain.state.contract.voption

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{Contract, ContractGenHelper, ContractVOption, DataEntry, DataType}
import vsys.blockchain.contract.voption.VOptionContractGen
import vsys.blockchain.state.diffs.assertDiffAndState
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.transaction.contract.RegisterContractTransaction

class RegisterVOptionContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with VOptionContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val languageCode: String = "vdds"
  val languageVersion: Int = 2

  val preconditionAndBuildVOptionContract: Gen[(Array[Byte], Array[Byte], Seq[Array[Byte]],
    Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- ContractGenHelper.languageCodeGen(languageCode)
    langVer <- ContractGenHelper.languageVersionGen(languageVersion)
    init <- Gen.const(ContractVOption.contract.trigger)
    descriptor <- Gen.const(ContractVOption.contract.descriptor)
    stateVar <- Gen.const(ContractVOption.contract.stateVar)
    stateMap <- Gen.const(ContractVOption.contract.stateMap)
    textual <- Gen.const(ContractVOption.contract.textual)
  } yield (langCode, langVer, init, descriptor, stateVar, stateMap, textual)

  property("register v-option contract build doesn't break invariant"){
    forAll(preconditionAndBuildVOptionContract) { case (langCode, langVer, init, descriptor, stateVar, stateMap, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, stateMap, textual) shouldBe an[Right[_, _]]
    }
  }

  val validContract: Gen[Contract] = vOptionContractGen()
  val preconditionsAndRegContractTest: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisVOptionGen(master, ts)
    contract <- validContract
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data: Seq[DataEntry] <- initVOptionDataStackGen(tokenId.arr, tokenId.arr, tokenId.arr, tokenId.arr, ts + 10,ts + 1000)
    description <- validDescStringGen
    create <- registerVOptionGen(master, contract, data, description, fee, ts + 1)
  } yield (genesis, create)

  property("register v-option contract transaction doesn't break invariant") {
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
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractVOption.contract))

        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
      }
    }
  }
}