package vsys.state.diffs

//import cats.Monoid
import com.wavesplatform.TransactionGen
//import com.wavesplatform.state2._
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
//import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import vsys.transaction.contract.RegisterContractTransaction
//import com.wavesplatform.state2.diffs._
import vsys.contract.DataEntry
//import vsys.transaction.proof.EllipticCurve25519Proof

class ContractTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

//  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionsAndContract: Gen[(GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- contractGen
    contract2 <- contractGen
    data: Seq[DataEntry] <- dataEntryGen
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create1: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, data, description, fee, feeScale, ts + 1).right.get
    create2: RegisterContractTransaction = RegisterContractTransaction.create(master, contract2, data, description, fee, feeScale, ts + 2).right.get
  } yield (genesis, create1, create2, create1.fee)

//  property("register contract transaction doesn't break invariant") {
//    forAll(preconditionsAndContract) { case (genesis, create, _, feeCreate) =>
//      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(create))) { (blockDiff, newState) =>
//        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
//        totalPortfolioDiff.balance shouldBe -feeCreate
//        totalPortfolioDiff.effectiveBalance shouldBe -feeCreate
//        newState.accountTransactionIds(EllipticCurve25519Proof.fromBytes(create.proofs.proofs.head.bytes.arr).toOption.get.publicKey, 2).size shouldBe 2 // genesis and create
//      }
//    }
//  }

}
