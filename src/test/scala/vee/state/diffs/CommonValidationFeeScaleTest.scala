package vee.state.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.Shrink
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vee.transaction.spos.ContendSlotsTransaction
import scorex.transaction.{ValidationError, PaymentTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

class CommonValidationFeeScaleTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  // TODO
  // later change to Payment and random invalid feeScale
  property("disallows invalid fee scale") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      slotId <- slotidGen
      ts <- timestampGen
    } yield (master, fee, slotId, ts)) { case (master, fee, slotId, ts) =>
      ContendSlotsTransaction.create(master, slotId, fee, 101, ts) shouldEqual(Left(ValidationError.WrongFeeScale(101)))
    }
  }

  property("Payment transaction") {
    forAll(for {
      sender <- accountGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      ts <- timestampGen
    } yield (sender, recipient, amount, fee, ts)) { case (sender, recipient, amount, fee, ts) =>
      PaymentTransaction.create(sender, recipient, amount, fee, 101, ts) shouldEqual(Left(ValidationError.WrongFeeScale(101)))
    }
  }

  property("Lease transaction") {
    forAll(for {
      sender <- accountGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      ts <- timestampGen
    } yield (sender, recipient, amount, fee, ts)) { case (sender, recipient, amount, fee, ts) =>
      LeaseTransaction.create(sender, amount, fee, 101, ts, recipient) shouldEqual(Left(ValidationError.WrongFeeScale(101)))
    }
  }

  property("Lease cancel transaction") {
    forAll(for {
      leaseSender <- accountGen
      leaseRecipient <- accountGen
      leaseAmount <- positiveLongGen
      leaseFee <- smallFeeGen
      leaseFeeScale <- feeScaleGen
      leaseTimestamp <- timestampGen
      lease = LeaseTransaction.create(leaseSender, leaseAmount, leaseFee, leaseFeeScale, leaseTimestamp, leaseRecipient).right.get
    } yield (lease, leaseSender)) { case (lease, leaseSender) =>
      LeaseCancelTransaction.create(leaseSender, lease.id, lease.fee, 101, lease.timestamp + 1) shouldEqual(Left(ValidationError.WrongFeeScale(101)))
    }
  }

  // TODO
  // test signed transactions

}
