package vsys.blockchain.state.contract.token

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.state.{ByteStr, EitherExt2, Portfolio}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.transaction.contract._
import vsys.utils.serialization.Deser

class RegisterTokenContractTransactionDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val languageCode: String = "vdds"
  val languageVersion: Int = 1

  val preconditionAndBuildRegContract: Gen[(Array[Byte], Array[Byte], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- ContractGenHelper.languageCodeGen(languageCode)
    langVer <- ContractGenHelper.languageVersionGen(languageVersion)
    init <- Gen.const(ContractPermitted.contract.trigger)
    descriptor <- Gen.const(ContractPermitted.contract.descriptor)
    descriptorWithoutSplit <- Gen.const(ContractPermitted.contractWithoutSplit.descriptor)
    stateVar <- Gen.const(ContractPermitted.contract.stateVar)
    textual <- Gen.const(ContractPermitted.contract.textual)
    textualWithoutSplit <- Gen.const(ContractPermitted.contractWithoutSplit.textual)
  } yield (langCode, langVer, init, descriptor, descriptorWithoutSplit, stateVar, textual, textualWithoutSplit)

  property("register token contract build doesn't break invariant"){
    forAll(preconditionAndBuildRegContract) { case (langCode, langVer, init, descriptor, descriptorWithoutSplit, stateVar, textual, textualWithoutSplit) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, Seq(), textual) shouldBe an[Right[_, _]]
      Contract.buildContract(langCode, langVer, init, descriptorWithoutSplit, stateVar, Seq(), textualWithoutSplit) shouldBe an[Right[_, _]]
    }
  }

  val validContract: Gen[Contract] = tokenContractGen(true)
  val preconditionsAndRegContractTest: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisTokenGen(master, ts)
    contract <- validContract
    data: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    create <- registerTokenGen(master, contract, data, description, fee, ts + 1)
  } yield (genesis, create)

  property("register token contract transaction doesn't break invariant") {
    forAll(preconditionsAndRegContractTest) { case (genesis, reg: RegisterContractTransaction) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(reg))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -reg.transactionFee
        totalPortfolioDiff.effectiveBalance shouldBe -reg.transactionFee
        val master = reg.proofs.firstCurveProof.toOption.get.publicKey
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

        val (_, masterTxs) = newState.accountTransactionIds(master, 2, 0)
        masterTxs.size shouldBe 2 // genesis, reg
        newState.contractTokens(contractId) shouldBe 1
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractPermitted.contract))

        newState.contractInfo(issuerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.tokenInfo(maxKey) shouldEqual Some(DataEntry(Longs.toByteArray(100000000L), DataType.Amount))
        newState.tokenAccountBalance(totalKey) shouldBe 0L
        newState.tokenInfo(unityKey) shouldEqual Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.tokenInfo(descKey) shouldEqual Some(DataEntry.create(descDE, DataType.ShortText).explicitGet())
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
      }
    }
  }

}
