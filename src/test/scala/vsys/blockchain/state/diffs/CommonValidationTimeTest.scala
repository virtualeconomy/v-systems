package vsys.blockchain.state.diffs

import vsys.blockchain.transaction.TransactionGen
import vsys.blockchain.state._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.settings.TestFunctionalitySettings
import vsys.blockchain.transaction.PaymentTransaction

class CommonValidationTimeTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  property("disallows too old transacions") {
    forAll(for {
      prevBlockTs <- timestampGen
      blockTs <- Gen.choose(prevBlockTs, prevBlockTs + 7 * 24 * 3600 * 1000)
      master <- accountGen
      height <- positiveIntGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      attachment <- attachmentGen
      // feeScale <- positiveShortGen
      transfer1 = PaymentTransaction.create(master, recipient, amount, fee, 100, prevBlockTs - CommonValidation.MaxTimePrevBlockOverTransactionDiff.toNanos - 1, attachment).explicitGet()
    } yield (prevBlockTs, blockTs, height, transfer1)) { case (prevBlockTs, blockTs, height, transfer1) =>
      TransactionDiffer(TestFunctionalitySettings.Enabled, Some(prevBlockTs), blockTs, height)(newState(), transfer1) should produce("too old")
    }
  }

  property("disallows transactions from far future") {
    forAll(for {
      prevBlockTs <- timestampGen
      blockTs <- Gen.choose(prevBlockTs, prevBlockTs + 7 * 24 * 3600 * 1000)
      master <- accountGen
      height <- positiveIntGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      //feeScale <- positiveShortGen
      attachment <- attachmentGen
      transfer1 = PaymentTransaction.create(master, recipient, amount, fee, 100, blockTs + CommonValidation.MaxTimeTransactionOverBlockDiff.toNanos + 1, attachment).explicitGet()
    } yield (prevBlockTs, blockTs, height, transfer1)) { case (prevBlockTs, blockTs, height, transfer1) =>
      TransactionDiffer(TestFunctionalitySettings.Enabled, Some(prevBlockTs), blockTs, height)(newState(), transfer1) should produce("far future")
    }
  }
}
