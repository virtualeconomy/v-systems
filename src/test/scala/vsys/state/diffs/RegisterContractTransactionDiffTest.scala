package vsys.state.diffs

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.{ByteStr, Portfolio}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import com.wavesplatform.state2.diffs._
import scorex.serialization.Deser
import vsys.contract._
import vsys.transaction.proof.EllipticCurve25519Proof
import vsys.transaction.contract._


class RegisterContractTransactionDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with OpcFunction
  with TransactionGen
  with ContractGen
  with StateVar
  with Texture
  with DataStack {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionAndBuildRegContract: Gen[(Array[Byte], Array[Byte], Array[Byte], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- languageCodeFromLengthGen("vdds")
    langVer <- languageVersionFromLengthGen(1)
    init <- initFunGen()
    descriptor <- descriptorFullGen()
    stateVar <- stateVarRandomGen()
    textual <- textureRightGen
  } yield (langCode, langVer, init, descriptor, stateVar, textual)

  property("register contract build doesn't break invariant"){
    forAll(preconditionAndBuildRegContract) { case (langCode, langVer, init, descriptor, stateVar, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, textual) shouldBe an[Right[_, _]]
    }
  }

  val languageCode: String = "vdds"
  val languageVersion: Int = 1
  val validContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndRegContractTest: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    master <- accountGen
    ts <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    contract <- validContract
    data: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    create: RegisterContractTransaction = RegisterContractTransaction.create(master, contract, data, description, fee, feeScale, ts + 1).right.get
  } yield (genesis, create)

  property("register contract transaction doesn't break invariant") {
    forAll(preconditionsAndRegContractTest) { case (genesis, reg) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(reg))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -reg.fee
        totalPortfolioDiff.effectiveBalance shouldBe -reg.fee
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        val contractId = reg.contractId.bytes
        val tokenId = ByteStr(Bytes.concat(contractId.arr, Ints.toByteArray(0)))
        val issuerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))
        val maxKey = ByteStr(Bytes.concat(tokenId.arr, Array(0.toByte)))
        val totalKey = ByteStr(Bytes.concat(tokenId.arr, Array(1.toByte)))
        val unityKey = ByteStr(Bytes.concat(tokenId.arr, Array(2.toByte)))
        val descKey = ByteStr(Bytes.concat(tokenId.arr, Array(3.toByte)))
        val descDE = Deser.serilizeString("init")
        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, master.toAddress.bytes.arr))

        newState.accountTransactionIds(master, 2).size shouldBe 2 // genesis, reg
        newState.contractTokens(contractId) shouldBe 1
        newState.contractContent(contractId).get._1 shouldBe 2
        newState.contractContent(contractId).get._2.arr shouldEqual reg.id.arr
        newState.contractContent(contractId).get._3.bytes.arr shouldEqual ContractPermitted.contract.bytes.arr
        newState.contractInfo(issuerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.contractInfo(makerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.tokenInfo(maxKey).get.bytes shouldEqual DataEntry(Longs.toByteArray(100000000L), DataType.Amount).bytes
        newState.tokenAccountBalance(totalKey) shouldBe 0L
        newState.tokenInfo(unityKey).get.bytes shouldEqual DataEntry(Longs.toByteArray(100L), DataType.Amount).bytes
        newState.tokenInfo(descKey).get.bytes shouldEqual DataEntry.create(descDE, DataType.ShortText).right.get.bytes
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 0L
      }
    }
  }

}
