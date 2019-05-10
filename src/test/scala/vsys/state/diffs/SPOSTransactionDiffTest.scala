package vsys.state.diffs

import cats.Monoid
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import vsys.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.TransactionDiffer._
import scorex.transaction.ValidationError
import scorex.settings.TestFunctionalitySettings
import vsys.transaction.proof.{EllipticCurve25519Proof, Proofs}

class SPOSTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionsAndContend: Gen[(GenesisTransaction, ContendSlotsTransaction, ContendSlotsTransaction, ContendSlotsTransaction, ContendSlotsTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    slotid <- slotidGen
    slotid2 <- slotidGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    contend: ContendSlotsTransaction <- contendGeneratorP(master, slotid)
    contendMultiSlots: ContendSlotsTransaction <- contendGeneratorP(master, slotid2)
    contendInvalidId1: ContendSlotsTransaction <- contendGeneratorP(master, -1)
    contendInvalidId2: ContendSlotsTransaction <- contendGeneratorP(master, TestFunctionalitySettings.Enabled.numOfSlots)
  } yield (genesis, contend, contendMultiSlots, contendInvalidId1, contendInvalidId2, contend.fee)

  property("contend transaction doesn't break invariant") {
    forAll(preconditionsAndContend) { case (genesis, contend, _, _, _, feeContend) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(contend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -feeContend
        totalPortfolioDiff.effectiveBalance shouldBe -feeContend
        val sender = EllipticCurve25519Proof.fromBytes(contend.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        newState.accountTransactionIds(sender, 2).size shouldBe 2 // genesis and payment
      }
    }
  }

  property("contend transaction fail, when sender already own a slot") {
    forAll(preconditionsAndContend) { case (genesis, contend, contendM, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(contend, contendM))) { blockDiffEi =>
        blockDiffEi should produce("already owned one slot or contended by other node")
      }
    }
  }

  property("contend transaction can not contend invalid slots") {
    forAll(preconditionsAndContend) { case (genesis, _, _, invalid1:ContendSlotsTransaction, invalid2:ContendSlotsTransaction, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(invalid1))) { blockDiffEi =>
        blockDiffEi shouldBe Left(TransactionValidationError(ValidationError.InvalidSlotId(invalid1.slotId), invalid1))
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(invalid2))) { blockDiffEi =>
        blockDiffEi shouldBe Left(TransactionValidationError(ValidationError.InvalidSlotId(invalid2.slotId), invalid2))
      }
    }
  }

  val preconditionsAndContendx: Gen[(GenesisTransaction, GenesisTransaction,
    ContendSlotsTransaction, ContendSlotsTransaction, ContendSlotsTransaction, Long)] = for {
    master <- accountGen
    master1 <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    genesis1: GenesisTransaction = GenesisTransaction.create(master1, 2 * ENOUGH_AMT, -1, ts).right.get
    contend: ContendSlotsTransaction <- contendGeneratorP(ts + 1, master, 0)
    contend1: ContendSlotsTransaction <- contendGeneratorP(ts + 2, master1, 0)
    contend2: ContendSlotsTransaction <- contendGeneratorP(ts + 3, master, 1)
  } yield (genesis, genesis1, contend, contend1, contend2, contend.fee)


  property("node contended by others can not contend a new slot") {
    forAll(preconditionsAndContendx) { case (genesis, genesis1, contend, contend1, contend2, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1)), TestBlock.create(Seq(contend))), TestBlock.create(Seq(contend1))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis1)), TestBlock.create(Seq(contend)), TestBlock.create(Seq(contend1))), TestBlock.create(Seq(contend2))) { blockDiffEi =>
        blockDiffEi should produce("already owned one slot or contended by other node")
      }
    }
  }

  // the num of slot is 2
  val preconditionsAndRelease: Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction, GenesisTransaction,
    GenesisTransaction, GenesisTransaction, GenesisTransaction, GenesisTransaction, GenesisTransaction, GenesisTransaction,
    GenesisTransaction, ReleaseSlotsTransaction, ReleaseSlotsTransaction, ReleaseSlotsTransaction, ReleaseSlotsTransaction, Long)] = for {
    master0 <- accountGen
    master1 <- accountGen
    master2 <- accountGen
    master3 <- accountGen
    master4 <- accountGen
    master5 <- accountGen
    master6 <- accountGen
    master7 <- accountGen
    master8 <- accountGen
    master9 <- accountGen
    master10 <- accountGen

    ts1 <- positiveIntGen
    genesis0: GenesisTransaction = GenesisTransaction.create(master0, ENOUGH_AMT, 0, ts1).right.get
    genesis1: GenesisTransaction = GenesisTransaction.create(master1, ENOUGH_AMT, 4, ts1).right.get
    genesis2: GenesisTransaction = GenesisTransaction.create(master2, ENOUGH_AMT, 8, ts1).right.get
    genesis3: GenesisTransaction = GenesisTransaction.create(master3, ENOUGH_AMT, 12, ts1).right.get
    genesis4: GenesisTransaction = GenesisTransaction.create(master4, ENOUGH_AMT, 16, ts1).right.get
    genesis5: GenesisTransaction = GenesisTransaction.create(master5, ENOUGH_AMT, 20, ts1).right.get
    genesis6: GenesisTransaction = GenesisTransaction.create(master6, ENOUGH_AMT, 24, ts1).right.get
    genesis7: GenesisTransaction = GenesisTransaction.create(master7, ENOUGH_AMT, 28, ts1).right.get
    genesis8: GenesisTransaction = GenesisTransaction.create(master8, ENOUGH_AMT, 32, ts1).right.get
    genesis9: GenesisTransaction = GenesisTransaction.create(master9, ENOUGH_AMT, 36, ts1).right.get
    genesis10: GenesisTransaction = GenesisTransaction.create(master10, ENOUGH_AMT, 40, ts1).right.get

    release1: ReleaseSlotsTransaction <- releaseGeneratorP(master0, 0)
    releaseInvalid1: ReleaseSlotsTransaction <- releaseGeneratorP(master0, 4)
    releaseInvalid2: ReleaseSlotsTransaction <- releaseGeneratorP(master0, -1)
    releaseInvalid3: ReleaseSlotsTransaction <- releaseGeneratorP(master0, TestFunctionalitySettings.Enabled.numOfSlots)

  } yield (genesis0, genesis1, genesis2, genesis3, genesis4, genesis5, genesis6, genesis7, genesis8, genesis9, genesis10,
    release1, releaseInvalid1, releaseInvalid2, releaseInvalid3, release1.fee)

  property("release transaction doesn't break invariant") {
    forAll(preconditionsAndRelease) { case (genesis0, genesis1, genesis2, genesis3, genesis4, genesis5, genesis6,
    genesis7, genesis8, genesis9, genesis10, release1, _, _, _, f1) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis0, genesis1, genesis2, genesis3, genesis4,
        genesis5, genesis6, genesis7, genesis8, genesis9, genesis10))), TestBlock.create(Seq(release1))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe - f1
        totalPortfolioDiff.effectiveBalance shouldBe -f1
        val sender = EllipticCurve25519Proof.fromBytes(release1.proofs.proofs.head.bytes.arr).toOption.get.publicKey
        newState.accountTransactionIds(sender, 10).size shouldBe 2 // genesis and release
      }
    }
  }

  property("release transaction can not release wrong slot id") {
    forAll(preconditionsAndRelease) { case (genesis0, genesis1, genesis2, genesis3, genesis4, genesis5, genesis6,
    genesis7, genesis8, genesis9, genesis10, _, invalid1, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis0, genesis1, genesis2, genesis3, genesis4,
        genesis5, genesis6, genesis7, genesis8, genesis9, genesis10))), TestBlock.create(Seq(invalid1))) { blockDiffEi =>
        blockDiffEi should produce("can not release the minting right of slot id")
      }
    }
  }

  property("release transaction can not release invalid slots") {
    forAll(preconditionsAndRelease) { case (genesis0, genesis1, genesis2, genesis3, genesis4, genesis5, genesis6,
    genesis7, genesis8, genesis9, genesis10, _, _, invalid2:ReleaseSlotsTransaction, invalid3:ReleaseSlotsTransaction, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis0, genesis1, genesis2, genesis3, genesis4,
        genesis5, genesis6, genesis7, genesis8, genesis9, genesis10))), TestBlock.create(Seq(invalid2))) { blockDiffEi =>
        blockDiffEi shouldBe Left(TransactionValidationError(ValidationError.InvalidSlotId(invalid2.slotId), invalid2))
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis0, genesis1, genesis2, genesis3, genesis4,
        genesis5, genesis6, genesis7, genesis8, genesis9, genesis10))), TestBlock.create(Seq(invalid3))) { blockDiffEi =>
        blockDiffEi shouldBe Left(TransactionValidationError(ValidationError.InvalidSlotId(invalid3.slotId), invalid3))
      }
    }
  }

  property("release transaction can not release when minter number is not enough") {
    forAll(preconditionsAndRelease) { case (genesis0, genesis1, genesis2, genesis3, genesis4, genesis5, genesis6,
    genesis7, genesis8, _, _, release1, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis0, genesis1, genesis2, genesis3, genesis4,
        genesis5, genesis6, genesis7, genesis8))), TestBlock.create(Seq(release1))) { blockDiffEi =>
        blockDiffEi should produce("effective slot address(es) left, can not release the minting right")
      }
    }
  }

  val preconditionsAndContend2: Gen[(GenesisTransaction, ContendSlotsTransaction)] = for {
    master <- accountGen
    ts <- positiveIntGen
    slotId <- slotidGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    contendTmp: ContendSlotsTransaction <- contendGeneratorP(master, slotId)
    proof = contendTmp.proofs.proofs.head
    proofs = Proofs(List(proof, proof)) // two proofs case
    contend = contendTmp.copy(proofs = proofs)
  } yield (genesis, contend)

  property("contend transaction can not contain multi-proofs") {
    forAll(preconditionsAndContend2) { case (genesis, contend) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(contend))) { blockDiffEi =>
        blockDiffEi should produce("Too many proofs")
      }
    }
  }

  val preconditionsAndRelease2: Gen[(GenesisTransaction, ContendSlotsTransaction, ReleaseSlotsTransaction)] = for {
    master <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    contend: ContendSlotsTransaction <- contendGeneratorP(master, 0)
    releaseTmp: ReleaseSlotsTransaction <- releaseGeneratorP(master, 0)
    proof = releaseTmp.proofs.proofs.head
    proofs = Proofs(List(proof, proof)) // two proofs case
    release = releaseTmp.copy(proofs = proofs)
  } yield (genesis, contend, release)

  property("release transaction can not contain multi-proofs") {
    forAll(preconditionsAndRelease2) { case (genesis, contend, release) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(contend))), TestBlock.create(Seq(release))) { blockDiffEi =>
        blockDiffEi should produce("Too many proofs")
      }
    }
  }

}
