package vee.state.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.Shrink
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vee.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import scorex.transaction.PaymentTransaction
import scorex.transaction.ValidationError
import vee.database.Entry
import vee.transaction.database.DbPutTransaction

class CommonValidationFeeScaleTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  property("disallows invalid fee scale in ContendSlots Transaction") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      slotId <- slotidGen
      ts <- timestampGen
    } yield (master, fee, slotId, ts)) { case (master, fee, slotId, ts) =>
      ContendSlotsTransaction.create(master, slotId, fee, 101, ts) shouldEqual Left(ValidationError.WrongFeeScale(101))
    }
  }

  property("disallows invalid fee scale in ReleaseSlots Transaction") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      slotId <- slotidGen
      ts <- timestampGen
    } yield (master, fee, slotId, ts)) { case (master, fee, slotId, ts) =>
      ReleaseSlotsTransaction.create(master, slotId, fee, 99, ts) shouldEqual Left(ValidationError.WrongFeeScale(99))
    }
  }

  property("disallows invalid fee scale in Payment Transaction") {
    forAll(for {
      master <- accountGen
      recipient <- otherAccountGen(candidate = master)
      amount <- positiveLongGen
      fee <- smallFeeGen
      ts <- timestampGen
    } yield (master, fee, recipient, amount, ts)) { case (master, fee, recipient, amount, ts) =>
      PaymentTransaction.create(master, recipient, amount, fee, 0, ts, Array()) shouldEqual Left(ValidationError.WrongFeeScale(0))
    }
  }

  property("disallows invalid fee scale in Lease transaction") {
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

  property("disallows invalid fee scale in Lease cancel transaction") {
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

  property("disallows invalid fee scale for DbPutTransaction") {
    forAll(for {
      timestamp <- positiveLongGen
      sender <- accountGen
      nameg <- validAliasStringGen
      entry <- entryGen
      fee <- smallFeeGen
    } yield (timestamp, sender, name, entry, fee)) { case (timestamp, sender, name, entry, fee) =>
      DbPutTransaction.create(sender, name, entry, fee, 101, timestamp) shouldEqual(Left(ValidationError.WrongFeeScale(101)))
    }
  }

  // TODO
  // test signed transactions

}
