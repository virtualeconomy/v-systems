package com.wavesplatform.state2.diffs

import cats.Monoid
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import vee.transaction.database.DbPutTransaction

class DbTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndDbPut: Gen[(GenesisTransaction, DbPutTransaction, Long)] = for {
    sender <- accountGen
    ts <- positiveIntGen
    fee: Long <- smallFeeGen
    genesis: GenesisTransaction = GenesisTransaction.create(sender, ENOUGH_AMT, -1, ts).right.get
    tx: DbPutTransaction <- dbPutGeneratorP(ts, sender, fee)
  } yield (genesis, tx, tx.fee)

  val preconditionsWithoutEnoughAmtAndDbPut: Gen[(GenesisTransaction, DbPutTransaction, Long)] = for {
    sender <- accountGen
    ts <- positiveIntGen
    fee: Long <- smallFeeGen
    genesis: GenesisTransaction = GenesisTransaction.create(sender, fee / 2, -1, ts).right.get
    tx: DbPutTransaction <- dbPutGeneratorP(ts, sender, fee)
  } yield (genesis, tx, tx.fee)


  property("Diff doesn't break invariant") {
    forAll(preconditionsAndDbPut) { case ((genesis, dbPutTx, feePayment)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(dbPutTx))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -feePayment
        totalPortfolioDiff.effectiveBalance shouldBe -feePayment
        totalPortfolioDiff.spendableBalance shouldBe -feePayment
        newState.accountTransactionIds(dbPutTx.sender, 2).size shouldBe 2 // genesis and dbPut transaction
      }
    }
  }

  property("Insufficient amount") {
    forAll(preconditionsWithoutEnoughAmtAndDbPut) { case ((genesis, dbPutTx, feePayment)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(dbPutTx))) { blockDiffEi =>
        blockDiffEi should produce("negative vee balance")
      }
    }
  }
}
