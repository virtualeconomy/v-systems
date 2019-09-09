package vsys.blockchain.state.diffs

import cats._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import vsys.blockchain.state._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.{Address, PrivateKeyAccount}
import vsys.blockchain.block.TestBlock
import vsys.settings.TestFunctionalitySettings
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.{GenesisTransaction, PaymentTransaction, TransactionGen, TransactionParser, ValidationError}
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proof, Proofs}
import vsys.blockchain.state.diffs.CommonValidation.MaxTimeTransactionOverBlockDiff

class LeaseTransactionsDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  def total(l: LeaseInfo): Long = l.leaseIn - l.leaseOut

  property("can not lease to self") {
    val selfLease: Gen[(GenesisTransaction, LeaseTransaction)] = for {
      master: PrivateKeyAccount <- accountGen
      amount <- positiveLongGen
      fee <- smallFeeGen
      feeScale <- feeScaleGen
      ts <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()

      toSign = Bytes.concat(Array(TransactionParser.TransactionType.LeaseTransaction.id.toByte),
        master.toAddress.bytes.arr,
        Longs.toByteArray(amount),
        Longs.toByteArray(fee),
        Shorts.toByteArray(feeScale),
        Longs.toByteArray(ts + 1))

      proof: Proof = EllipticCurve25519Proof.createProof(toSign, master)
      proofs: Proofs =  Proofs.create(List(proof.bytes)).explicitGet()
      lease: LeaseTransaction = LeaseTransaction(amount, fee, feeScale, ts + 1, master.toAddress, proofs)

    } yield (genesis, lease)

    forAll(selfLease) { case (genesis, lease) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(lease))) { totalDiffEi =>
        totalDiffEi should produce("Cannot lease to self")
      }
    }

    forAll(selfLease) { case (_, lease: LeaseTransaction) =>
      LeaseTransaction.createWithProof(lease.amount, lease.transactionFee, lease.feeScale, lease.timestamp, lease.recipient, lease.proofs) shouldBe Left(ValidationError.ToSelf)
    }

  }

  property("can lease/cancel lease preserving vsys invariant") {

    val sunnyDayLeaseLeaseCancel: Gen[(GenesisTransaction, LeaseTransaction, LeaseCancelTransaction, Long, Long)] = for {
      master <- accountGen
      recipient <- accountGen suchThat (_ != master)
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
      (lease, unlease) <- leaseAndCancelGeneratorP(master, recipient)
    } yield (genesis, lease, unlease, lease.transactionFee, unlease.transactionFee)

    forAll(sunnyDayLeaseLeaseCancel) { case ((genesis, lease, leaseCancel, feeLease, feeLeaseCancel)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(lease))) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -feeLease
        total(totalPortfolioDiff.leaseInfo) shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe -feeLease
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        totalDiff.snapshots(lease.recipient.asInstanceOf[Address]) shouldBe Map(2 -> Snapshot(0, 0, lease.amount, 0))
      }

      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, lease))), TestBlock.create(Seq(leaseCancel))) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -feeLeaseCancel
        total(totalPortfolioDiff.leaseInfo) shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe -feeLeaseCancel
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        totalDiff.snapshots(lease.recipient.asInstanceOf[Address]) shouldBe Map(2 -> Snapshot(1, 0, 0, 0))

        newState.accountPortfolio(EllipticCurve25519Proof.fromBytes(lease.proofs.proofs.head.bytes.arr).toOption.get.publicKey).leaseInfo shouldBe LeaseInfo.empty
        newState.accountPortfolio(lease.recipient.asInstanceOf[Address]).leaseInfo shouldBe LeaseInfo.empty
      }
    }
  }

  val cancelLeaseTwice: Gen[(GenesisTransaction, PaymentTransaction, LeaseTransaction, LeaseCancelTransaction, LeaseCancelTransaction, Long)] = for {
    master <- accountGen
    recpient <- accountGen suchThat (_ != master)
    blockTime <- timestampGen
    ts <- Gen.choose(blockTime, blockTime + MaxTimeTransactionOverBlockDiff.toNanos - 1)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    (lease, unlease) <- leaseAndCancelGeneratorP(master, recpient)
    fee2 <- smallFeeGen
    //feeScale: Short <- positiveShortGen //set to 100 in this version
    unlease2 = LeaseCancelTransaction.create(master, lease.id, fee2, 100, ts + 1).explicitGet()
    // ensure recipient has enough effective balance
    payment <- paymentGeneratorP(master, recpient) suchThat (_.amount > lease.amount)
  } yield (genesis, payment, lease, unlease, unlease2, blockTime)

  property("cannot cancel lease twice") {
    forAll(cancelLeaseTwice) {
      case ((genesis, payment, lease, leaseCancel, leaseCancel2, blockTime)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, payment, lease, leaseCancel))), TestBlock.create(blockTime, Seq(leaseCancel2)), TestFunctionalitySettings.Enabled) { totalDiffEi =>
          totalDiffEi should produce("Cannot cancel already cancelled lease")
        }
    }
  }

  property("cannot lease more than actual balance(cannot lease forward)") {
    val setup: Gen[(GenesisTransaction, LeaseTransaction, LeaseTransaction)] = for {
      master <- accountGen
      recipient <- accountGen suchThat (_ != master)
      forward <- accountGen suchThat (!Set(master, recipient).contains(_))
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
      (lease, _) <- leaseAndCancelGeneratorP(master, recipient)
      (leaseForward, _) <- leaseAndCancelGeneratorP(recipient, forward)
    } yield (genesis, lease, leaseForward)

    forAll(setup) { case ((genesis, lease, leaseForward)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, lease))), TestBlock.create(Seq(leaseForward)), TestFunctionalitySettings.Enabled) { totalDiffEi =>
        totalDiffEi should produce("Cannot lease more than own")
      }
    }
  }

  def cancelLeaseOfAnotherSender(unleaseByRecipient: Boolean): Gen[(GenesisTransaction, GenesisTransaction, LeaseTransaction, LeaseCancelTransaction, Long)] = for {
    master <- accountGen
    recipient <- accountGen suchThat (_ != master)
    other <- accountGen suchThat (_ != recipient)
    unleaser = if (unleaseByRecipient) recipient else other
    blockTime <- timestampGen
    ts <- Gen.choose(blockTime, blockTime + MaxTimeTransactionOverBlockDiff.toNanos - 1)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    genesis2: GenesisTransaction = GenesisTransaction.create(unleaser, ENOUGH_AMT, -1, ts).explicitGet()
    (lease, _) <- leaseAndCancelGeneratorP(master, recipient)
    fee2 <- smallFeeGen
    unleaseOtherOrRecipient = LeaseCancelTransaction.create(unleaser, lease.id, fee2, 100, ts + 1).explicitGet()
  } yield (genesis, genesis2, lease, unleaseOtherOrRecipient, blockTime)

  property("cannot cancel lease of another sender") {
    forAll(Gen.oneOf(true, false).flatMap(cancelLeaseOfAnotherSender)) {
      case ((genesis, genesis2, lease, unleaseOtherOrRecipient, blockTime)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseOtherOrRecipient)), TestFunctionalitySettings.Enabled) { totalDiffEi =>
          totalDiffEi should produce("LeaseTransaction was leased by other sender")
        }
    }
  }
}

