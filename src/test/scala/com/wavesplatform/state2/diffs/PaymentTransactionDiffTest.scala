package com.wavesplatform.state2.diffs

import cats.Monoid
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, PaymentTransaction}
import vsys.transaction.proof.EllipticCurve25519Proof

class PaymentTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayment: Gen[(GenesisTransaction, PaymentTransaction, Long)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    transfer: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, transfer, transfer.fee)


  property("Diff doesn't break invariant") {
    forAll(preconditionsAndPayment) { case ((genesis, payment, feePayment)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(payment))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        val sender = EllipticCurve25519Proof.fromBytes(payment.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        totalPortfolioDiff.balance shouldBe -feePayment
        totalPortfolioDiff.effectiveBalance shouldBe -feePayment
        newState.accountTransactionIds(sender.toAddress, 2, 0).size shouldBe 2 // genesis and payment
      }
    }
  }
}
