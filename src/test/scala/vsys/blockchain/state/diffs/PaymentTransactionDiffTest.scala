package vsys.blockchain.state.diffs

import cats.Monoid
import vsys.blockchain.state._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.transaction.{GenesisTransaction, PaymentTransaction, TransactionGen}
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof

class PaymentTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayment: Gen[(GenesisTransaction, PaymentTransaction, Long)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    transfer: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, transfer, transfer.fee)


  property("Diff doesn't break invariant") {
    forAll(preconditionsAndPayment) { case ((genesis, payment, feePayment)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(payment))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = EllipticCurve25519Proof.fromBytes(payment.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        totalPortfolioDiff.balance shouldBe -feePayment
        totalPortfolioDiff.effectiveBalance shouldBe -feePayment
        newState.accountTransactionIds(sender.toAddress, 2, 0)._2.size shouldBe 2 // genesis and payment
      }
    }
  }
}
