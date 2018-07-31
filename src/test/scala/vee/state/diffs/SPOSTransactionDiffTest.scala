package vee.state.diffs

import cats.Monoid
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import vee.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import com.wavesplatform.state2.diffs._
import scorex.settings.TestFunctionalitySettings

class SPOSTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionsAndContend: Gen[(GenesisTransaction, ContendSlotsTransaction, ContendSlotsTransaction, ContendSlotsTransaction, ContendSlotsTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    slotid <- slotidGen
    slotid2 <- slotidGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    contend: ContendSlotsTransaction <- contendGeneratorP(master, slotid)
    contendMultiSlots: ContendSlotsTransaction <- contendGeneratorP(master, slotid2)
    contendInvalidId1: ContendSlotsTransaction <- contendGeneratorP(master, -1)
    contendInvalidId2: ContendSlotsTransaction <- contendGeneratorP(master, TestFunctionalitySettings.Enabled.numOfSlots)
  } yield (genesis, contend, contendMultiSlots, contendInvalidId1, contendInvalidId2, contend.fee)

  property("contend transaction doesn't break invariant") {
    forAll(preconditionsAndContend) { case ((genesis, contend, _, _, _, feeContend)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(contend))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -feeContend
        totalPortfolioDiff.effectiveBalance shouldBe -feeContend
        newState.accountTransactionIds(contend.sender, 2).size shouldBe 2 // genesis and payment
      }
    }
  }

  property("contend transaction fail, when sender already own a slot") {
    forAll(preconditionsAndContend) { case ((genesis, contend, contendM, _, _, _)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(contend, contendM))) { blockDiffEi =>
        blockDiffEi should produce("already own one slot.")
      }
    }
  }

  property("contend transaction can not contend invalid slots") {
    forAll(preconditionsAndContend) { case ((genesis, _, _, invalid1, invalid2, _)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(invalid1))) { blockDiffEi =>
        blockDiffEi should produce("invalid.")
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(invalid2))) { blockDiffEi =>
        blockDiffEi should produce("invalid.")
      }
    }
  }

  // the num of slot is 2
  val preconditionsAndRelease: Gen[(GenesisTransaction, GenesisTransaction, ContendSlotsTransaction, ContendSlotsTransaction,
    ReleaseSlotsTransaction, ReleaseSlotsTransaction, ReleaseSlotsTransaction, ReleaseSlotsTransaction, Long, Long, Long)] = for {
    master1 <- accountGen
    ts1 <- positiveIntGen
    genesis1: GenesisTransaction = GenesisTransaction.create(master1, ENOUGH_AMT, ts1).right.get
    contend1: ContendSlotsTransaction <- contendGeneratorP(master1, 0)
    release1: ReleaseSlotsTransaction <- releaseGeneratorP(master1, 0)
    releaseInvalid1: ReleaseSlotsTransaction <- releaseGeneratorP(master1, 1)
    releaseInvalid2: ReleaseSlotsTransaction <- releaseGeneratorP(master1, -1)
    releaseInvalid3: ReleaseSlotsTransaction <- releaseGeneratorP(master1, TestFunctionalitySettings.Enabled.numOfSlots)
    master2 <- accountGen
    ts2 <- positiveIntGen
    genesis2: GenesisTransaction = GenesisTransaction.create(master2, ENOUGH_AMT, ts2).right.get
    contend2: ContendSlotsTransaction <- contendGeneratorP(master2, 1)
  } yield (genesis1, genesis2, contend1, contend2, release1, releaseInvalid1, releaseInvalid2, releaseInvalid3, contend1.fee, contend2.fee, release1.fee)

  property("release transaction doesn't break invariant") {
    forAll(preconditionsAndRelease) { case ((genesis1, genesis2, contend1, contend2, release1, _, _, _, f1, f2, f3)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis1, genesis2))), TestBlock.create(Seq(contend1, contend2, release1))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -(f1 + f2 + f3)
        totalPortfolioDiff.effectiveBalance shouldBe -(f1 + f2 + f3)
        newState.accountTransactionIds(release1.sender, 2).size shouldBe 2 // genesis and payment
      }
    }
  }

  property("release transaction can not release wrong slot id") {
    forAll(preconditionsAndRelease) { case ((genesis1, genesis2, contend1, contend2, _, invalid1, invalid2, invalid3, _, _, _)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis1, genesis2))), TestBlock.create(Seq(contend1, contend2, invalid1))) { blockDiffEi =>
        blockDiffEi should produce("can not release the minting right of slot id")
      }
    }
  }

  property("release transaction can not release invalid slots") {
    forAll(preconditionsAndRelease) { case ((genesis1, genesis2, contend1, contend2, _, invalid1, invalid2, invalid3, _, _, _)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis1, genesis2))), TestBlock.create(Seq(contend1, contend2, invalid2))) { blockDiffEi =>
        blockDiffEi should produce("invalid.")
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis1, genesis2))), TestBlock.create(Seq(contend1, contend2, invalid3))) { blockDiffEi =>
        blockDiffEi should produce("invalid.")
      }
    }
  }

  property("release transaction can not release when minter number is not enough") {
    forAll(preconditionsAndRelease) { case ((genesis1, _, contend1, _, release1, _, _, _, _, _, _)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis1))), TestBlock.create(Seq(contend1, release1))) { blockDiffEi =>
        blockDiffEi should produce("effective slot address(es) left, can not release the minting right")
      }
    }
  }
}
