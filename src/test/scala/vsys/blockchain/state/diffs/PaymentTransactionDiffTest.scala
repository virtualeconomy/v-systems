package vsys.blockchain.state.diffs

import cats.Monoid
import vsys.blockchain.state._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.transaction.{GenesisTransaction, PaymentTransaction, TransactionGen}

class PaymentTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayment: Gen[(GenesisTransaction, PaymentTransaction, Long)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    transfer: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, transfer, transfer.transactionFee)


  property("Diff doesn't break invariant") {
    forAll(preconditionsAndPayment) { case ((genesis, payment:PaymentTransaction, feePayment: Long)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(payment))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = payment.proofs.firstCurveProof.explicitGet().publicKey
        totalPortfolioDiff.balance shouldBe -feePayment
        totalPortfolioDiff.effectiveBalance shouldBe -feePayment
        val (_, senderTxs) = newState.accountTransactionIds(sender.toAddress, 2, 0)
        senderTxs.size shouldBe 2 // genesis and payment
      }
    }
  }
}
