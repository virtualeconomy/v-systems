package vsys.blockchain.state.contract.vswap

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.contract.{Contract, ContractGenHelper, ContractVSwap}
import vsys.blockchain.contract.vswap.VSwapContractGen
import vsys.blockchain.transaction.TransactionGen

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
}
