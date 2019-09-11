package vsys.blockchain.state.diffs

import cats.Monoid
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, MintingTransaction, TransactionGen}

class MintingTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndMinting: Gen[(GenesisTransaction, MintingTransaction, Long)] = for {
    master <- accountGen
    recipient <- mintingAddressGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    minting: MintingTransaction <- mintingGeneratorP(recipient, 2)
  } yield (genesis, minting, minting.amount)


  property("Diff doesn't break invariant") {
    forAll(preconditionsAndMinting) { case ((genesis, minting, amount)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(minting))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe amount
        totalPortfolioDiff.effectiveBalance shouldBe amount
      }
    }
  }
}
