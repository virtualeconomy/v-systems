package vee.state.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import vee.transaction.spos.ContendSlotsTransaction
import scorex.transaction.ValidationError
import vee.database.Entry
import vee.transaction.database.DbPutTransaction

class CommonValidationFeeScaleTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  // TODO
  // later change to Payment and random invalid feeScale
  property("disallows invalid fee scale for ContendSlotsTransaction") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      slotId <- slotidGen
      ts <- timestampGen
    } yield (master, fee, slotId, ts)) { case (master, fee, slotId, ts) =>
      ContendSlotsTransaction.create(master, slotId, fee, 101, ts) shouldEqual(Left(ValidationError.WrongFeeScale(101)))
    }
  }

  property("disallows invalid fee scale for DbPutTransaction") {
    forAll(for {
      timestamp: Long <- positiveLongGen
      sender: PrivateKeyAccount <- accountGen
      name: String <- validAliasStringGen
      entry: Entry <- entryGen
      fee: Long <- smallFeeGen
    } yield (timestamp, sender, name, entry, fee)) { case (timestamp, sender, name, entry, fee) =>
      DbPutTransaction.create(timestamp, sender, name, entry, fee, 101) shouldEqual(Left(ValidationError.WrongFeeScale(101)))
    }
  }

  // TODO
  // test signed transactions

}
