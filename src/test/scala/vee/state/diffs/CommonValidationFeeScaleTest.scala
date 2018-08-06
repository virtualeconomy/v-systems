package vee.state.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.Shrink
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vee.transaction.spos.ContendSlotsTransaction
import scorex.transaction.ValidationError

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

  // TODO
  // test signed transactions

}
