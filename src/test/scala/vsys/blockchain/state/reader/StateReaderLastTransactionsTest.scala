package vsys.blockchain.state.reader

import vsys.blockchain.state.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.state.EitherExt2
import vsys.blockchain.transaction._

class StateReaderLastTransactionsTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayment: Gen[(Seq[Transaction], PaymentTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
    transfer1: PaymentTransaction <- paymentGeneratorP(ts + 1, master, recipient)
    transfer2: PaymentTransaction <- paymentGeneratorP(ts + 2, master, recipient)
    preconditions: Seq[Transaction] = Seq(genesis, transfer1, transfer2)

    transfer3: PaymentTransaction <- paymentGeneratorP(ts + 3, master, recipient)
  } yield (preconditions, transfer3)

  private def txToProcessedTx(tx: Transaction): ProcessedTransaction =
    ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)

  property("accountTransactions sort results by 'fresh head' rule") {
    forAll(preconditionsAndPayment) { case ((pre: Seq[Transaction], payment: PaymentTransaction)) =>
      assertDiffAndState(Seq(TestBlock.create(pre)), TestBlock.create(Seq(payment))) { (blockDiff, newState) =>

        val sender = payment.proofs.firstCurveProof.explicitGet().publicKey
        newState.accountTransactions(sender, 1, 0)._2.map{case (h,tx) => tx} shouldBe Seq(txToProcessedTx(payment))
        val (_, txs) = newState.accountTransactions(sender, 1, 0)
        txs shouldEqual Seq((2, txToProcessedTx(payment)))
        val g = pre(0)
        val tx1 = pre(1)
        val tx2 = pre(2)
        val (_, txs1) = newState.accountTransactions(sender, 3, 0)
        txs1 shouldEqual Seq((2, txToProcessedTx(payment)), (1, txToProcessedTx(tx2)), (1, txToProcessedTx(tx1)))
        val (_, txs2) = newState.accountTransactions(sender, 10, 0)
        txs2 shouldEqual Seq((2, txToProcessedTx(payment)), (1, txToProcessedTx(tx2)), (1, txToProcessedTx(tx1)), (1, txToProcessedTx(g)))
      }
    }
  }
}
