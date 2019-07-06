package vsys.blockchain.state.diffs

import com.google.common.primitives.{Bytes, Longs, Shorts}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.state.diffs.TransactionDiffer.TransactionValidationError
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import vsys.blockchain.transaction.{GenesisTransaction, PaymentTransaction, TransactionGen}
import vsys.blockchain.transaction.database.DbPutTransaction
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.ValidationError.WrongFeeScale
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}
import vsys.utils.serialization.BytesSerializable

class CommonValidationFeeScaleTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  implicit class ConditionalAssert(v: Either[_, _]) {

    def shouldBeRightIf(cond: Boolean): Assertion = {
      if (cond) {
        v shouldBe an[Right[_, _]]
      } else {
        v shouldBe an[Left[_, _]]
      }
    }
  }

  def validFeeScale(feeScale: Short): Boolean = {
    feeScale == 100
  }

  property("disallows invalid fee scale in ContendSlots Transaction") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      slotId <- slotidGen
      ts <- timestampGen
      feeScale <- positiveShortGen
    } yield (master, fee, feeScale, slotId, ts)) { case (master, fee, feeScale: Short, slotId, ts) =>
      ContendSlotsTransaction.create(master, slotId, fee, feeScale, ts) shouldBeRightIf validFeeScale(feeScale)
    }
  }

  property("disallows invalid fee scale in ReleaseSlots Transaction") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      slotId <- slotidGen
      ts <- timestampGen
      feeScale <- positiveShortGen
    } yield (master, fee, feeScale, slotId, ts)) { case (master, fee, feeScale :Short, slotId, ts) =>
      ReleaseSlotsTransaction.create(master, slotId, fee, feeScale, ts) shouldBeRightIf validFeeScale(feeScale)
    }
  }

  property("disallows invalid fee scale in Payment Transaction") {
    forAll(for {
      master <- accountGen
      recipient <- otherAccountGen(candidate = master)
      amount <- positiveLongGen
      fee <- smallFeeGen
      ts <- timestampGen
      feeScale <- positiveShortGen
    } yield (master, fee, feeScale, recipient, amount, ts)) { case (master, fee, feeScale: Short, recipient, amount, ts) =>
      PaymentTransaction.create(master, recipient, amount, fee, feeScale, ts, Array()) shouldBeRightIf validFeeScale(feeScale)
    }
  }

  property("disallows invalid fee scale in Lease transaction") {
    forAll(for {
      sender <- accountGen
      recipient <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      ts <- timestampGen
      feeScale <- positiveShortGen
    } yield (sender, recipient, amount, fee, feeScale, ts)) { case (sender, recipient, amount, fee, feeScale: Short, ts) =>
      LeaseTransaction.create(sender, amount, fee, feeScale, ts, recipient) shouldBeRightIf validFeeScale(feeScale)
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
      feeScale <- positiveShortGen
    } yield (lease, leaseSender, feeScale)) { case (lease, leaseSender, feeScale: Short) =>
      LeaseCancelTransaction.create(leaseSender, lease.id, lease.fee, feeScale, lease.timestamp + 1) shouldBeRightIf validFeeScale(feeScale)
    }
  }

  property("disallows invalid fee scale for DbPutTransaction") {
    forAll(for {
      timestamp <- positiveLongGen
      sender <- accountGen
      name <- validAliasStringGen
      entry <- entryGen
      fee <- smallFeeGen
      feeScale <- positiveShortGen
    } yield (timestamp, sender, name, entry, fee, feeScale)) { case (timestamp, sender, name, entry, fee, feeScale: Short) =>
      DbPutTransaction.create(sender, name, entry, fee, feeScale, timestamp) shouldBeRightIf validFeeScale(feeScale)
    }
  }

  /*
  This part test signed transaction cases. When a bad node broadcast an invalid feeScale transaction to others,
  the invalid feeScale will be detected in TransactionDiff instead of transaction create
   */

  val preconditionsAndPayment: Gen[(GenesisTransaction, PaymentTransaction, Short)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    ts2 <- positiveLongGen
    amount <- positiveLongGen
    fee <- smallFeeGen
    feeScale <- positiveShortGen
    attachment <- attachmentGen

    toSign: Array[Byte] = {
      val timestampBytes = Longs.toByteArray(ts2)
      val amountBytes = Longs.toByteArray(amount)
      val feeBytes = Longs.toByteArray(fee)
      val feeScaleBytes = Shorts.toByteArray(feeScale)

      Bytes.concat(Array(TransactionType.PaymentTransaction.id.toByte),
        timestampBytes,
        amountBytes,
        feeBytes,
        feeScaleBytes,
        recipient.bytes.arr,
        BytesSerializable.arrayWithSize(attachment)
      )
    }

    proofs: Proofs = Proofs.create(List(EllipticCurve25519Proof.createProof(toSign, master).bytes)).getOrElse(Proofs.empty)

    transfer: PaymentTransaction = PaymentTransaction(recipient, amount, fee, feeScale, ts2, attachment, proofs)
  } yield (genesis, transfer, feeScale)

  property("disallows invalid fee scale in Signed Payment Transaction") {
    forAll(preconditionsAndPayment retryUntil(_._3 != 100)) {
      case (genesis, payment, feeScale: Short) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(payment))) { totalDiffEi =>
          totalDiffEi shouldBe Left(TransactionValidationError(WrongFeeScale(feeScale), payment))
        }
    }
  }

}
