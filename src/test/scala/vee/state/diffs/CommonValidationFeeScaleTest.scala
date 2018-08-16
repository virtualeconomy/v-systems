package vee.state.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.Shrink
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vee.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import scorex.transaction.PaymentTransaction
import scorex.transaction.ValidationError

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

  // TODO
  // test signed transactions

}
