package vsys.blockchain.state.contract.vescrow

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.vescrow.VEscrowContractGen
import vsys.blockchain.state.diffs.assertDiffAndState
import vsys.blockchain.state.{ByteStr, Portfolio, _}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}

class RegisterVEscrowContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with VEscrowContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val languageCode: String = "vdds"
  val languageVersion: Int = 2

  val preconditionsAndBuildVEscrowContract: Gen[(Array[Byte], Array[Byte], Seq[Array[Byte]],
    Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- ContractGenHelper.languageCodeGen(languageCode)
    langVer <- ContractGenHelper.languageVersionGen(languageVersion)
    init <- Gen.const(ContractVEscrow.contract.trigger)
    descriptor <- Gen.const(ContractVEscrow.contract.descriptor)
    stateVar <- Gen.const(ContractVEscrow.contract.stateVar)
    stateMap <- Gen.const(ContractVEscrow.contract.stateMap)
    textual <- Gen.const(ContractVEscrow.contract.textual)
  } yield (langCode, langVer, init, descriptor, stateVar, stateMap, textual)

  property("ensure v-escrow can be built as a valid contract") {
    forAll(preconditionsAndBuildVEscrowContract) { case (langCode, langVer, init, descriptor, stateVar, stateMap, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, stateMap, textual) shouldBe an[Right[_,_]]
    }
  }

  val validContract: Gen[Contract] = vEscrowContractGen()
  val preconditionsAndRegContractTest: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisVEscrowGen(master, ts)
    contract <- validContract
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data: Seq[DataEntry] <- initVEscrowDataStackGen(tokenId.arr, 1, 1)
    description <- validDescStringGen
    create <- registerVEscrowGen(master, contract, data, description, fee, ts + 1)
  } yield (genesis, create)

  property("register v-escrow contract transaction doesn't break invariant") {
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
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractVEscrow.contract))

        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
      }
    }
  }
}