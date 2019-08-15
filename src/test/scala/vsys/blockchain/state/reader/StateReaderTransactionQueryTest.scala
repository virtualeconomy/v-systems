package vsys.blockchain.state.reader

import vsys.blockchain.transaction.TransactionGen
import vsys.blockchain.state.diffs._
import vsys.account.PrivateKeyAccount
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.transaction.{GenesisTransaction, PaymentTransaction, Transaction}
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.{ProcessedTransaction, TransactionStatus}

class StateReaderTransactionQueryTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  private val txTypeCount = 10

  val preTxsAndNewTxs: Gen[(Seq[Transaction], Seq[Transaction], PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount)] = for {
    master <- accountGen
    recipient1 <- accountGen
    recipient2 <- accountGen
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    prePayments1: Seq[PaymentTransaction] <- Gen.sequence[Seq[PaymentTransaction], PaymentTransaction]((1 to txTypeCount).map(i => paymentGeneratorP(ts + i, master, recipient1)))
    prePayments2: Seq[PaymentTransaction] <- Gen.sequence[Seq[PaymentTransaction], PaymentTransaction]((1 to txTypeCount).map(i => paymentGeneratorP(ts + 10 + i, master, recipient2)))
    (preLeasesAndCancel1: Seq[(LeaseTransaction, LeaseCancelTransaction)]) <- Gen.sequence[Seq[(LeaseTransaction, LeaseCancelTransaction)], (LeaseTransaction, LeaseCancelTransaction)]((1 to txTypeCount).map(i => leaseAndCancelGeneratorP(master, recipient1, ts + 19 + i * 2)))
    (preLeasesAndCancel2: Seq[(LeaseTransaction, LeaseCancelTransaction)]) <- Gen.sequence[Seq[(LeaseTransaction, LeaseCancelTransaction)], (LeaseTransaction, LeaseCancelTransaction)]((1 to txTypeCount).map(i => leaseAndCancelGeneratorP(master, recipient2, ts + 39 + i * 2)))
    newPayments1: Seq[PaymentTransaction] <- Gen.sequence[Seq[PaymentTransaction], PaymentTransaction]((1 to txTypeCount).map(i => paymentGeneratorP(ts + 60 + i, master, recipient1)))
    newPayments2: Seq[PaymentTransaction] <- Gen.sequence[Seq[PaymentTransaction], PaymentTransaction]((1 to txTypeCount).map(i => paymentGeneratorP(ts + 60 + 10 + i, master, recipient2)))
    (newLeasesAndCancel1: Seq[(LeaseTransaction, LeaseCancelTransaction)]) <- Gen.sequence[Seq[(LeaseTransaction, LeaseCancelTransaction)], (LeaseTransaction, LeaseCancelTransaction)]((1 to txTypeCount).map(i => leaseAndCancelGeneratorP(master, recipient1, ts + 60 + 19 + i * 2)))
    (newLeasesAndCancel2: Seq[(LeaseTransaction, LeaseCancelTransaction)]) <- Gen.sequence[Seq[(LeaseTransaction, LeaseCancelTransaction)], (LeaseTransaction, LeaseCancelTransaction)]((1 to txTypeCount).map(i => leaseAndCancelGeneratorP(master, recipient2, ts + 60 + 39 + i * 2)))

    (preLeases1: Seq[LeaseTransaction], preLeaseCancels1: Seq[LeaseCancelTransaction]) = preLeasesAndCancel1.unzip
    (preLeases2: Seq[LeaseTransaction], preLeaseCancels2: Seq[LeaseCancelTransaction]) = preLeasesAndCancel2.unzip
    (newLeases1: Seq[LeaseTransaction], newLeaseCancels1: Seq[LeaseCancelTransaction]) = newLeasesAndCancel1.unzip
    (newLeases2: Seq[LeaseTransaction], newLeaseCancels2: Seq[LeaseCancelTransaction]) = newLeasesAndCancel2.unzip

    preTxs: Seq[Transaction] = Seq(genesis) ++ prePayments1 ++ prePayments2 ++ preLeases1 ++ preLeaseCancels1 ++ preLeases2 ++ preLeaseCancels2

    newTxs: Seq[Transaction] = newPayments1 ++ newPayments2 ++ newLeases1 ++ newLeaseCancels1 ++ newLeases2 ++ newLeaseCancels2
  } yield (preTxs, newTxs, master, recipient1, recipient2)

  private def txToProcessedTx(tx: Transaction): ProcessedTransaction =
    ProcessedTransaction(TransactionStatus.Success, tx.transactionFee, tx)

  property("txTypeAccountTransactions working properly") {
    forAll(preTxsAndNewTxs) { case (preTxs, newTxs, m, r1, r2) =>
      val Seq(processedPre: Seq[ProcessedTransaction], processedNew: Seq[ProcessedTransaction]) =
        Seq(preTxs, newTxs).map(_.map(txToProcessedTx))
      assertDiffAndState(Seq(TestBlock.create(preTxs)), TestBlock.create(newTxs)) { (blockDiff, newState) =>

        newState.txTypeAccountTransactions(TransactionType.GenesisTransaction, m, 1, 0)._2.map{case (h,tx) => tx} shouldBe processedPre.take(1)
        newState.txTypeAccountTransactions(TransactionType.GenesisTransaction, m, 10, 0)._2.map{case (h,tx) => tx} shouldBe processedPre.take(1)
        (1 to 20).foreach(i => { newState.txTypeAccountTransactions(TransactionType.PaymentTransaction, m, i, 0)._2.map{case (h,tx) => tx} shouldBe processedNew.slice(20 - i, 20).reverse })
        (20 to 39).foreach(i => { newState.txTypeAccountTransactions(TransactionType.PaymentTransaction, m, 1, i)._2.map{case (h,tx) => tx} shouldBe processedPre.slice(40 - i, 41 - i) })
        (0 to 9).foreach(i => { newState.txTypeAccountTransactions(TransactionType.LeaseTransaction, m, 1, i)._2.map{case (h,tx) => tx} shouldBe processedNew.slice(49 - i, 50 - i) })
        (21 to 30).foreach(i => {
          newState.txTypeAccountTransactions(TransactionType.LeaseTransaction, m, i, 0)._2.map{case (h,tx) => tx} shouldBe (processedPre.slice(71 - i, 51) ++ processedNew.slice(20, 30) ++ processedNew.slice(40, 50)).reverse
        })
        
        newState.txTypeAccountTransactions(TransactionType.LeaseCancelTransaction, r1, 10, 5) match {
          case (c, txsWithHeight) => {
            val txs = txsWithHeight.toSeq.map{_._2}
            Seq(c, txs) shouldBe Seq(20, (processedPre.slice(36, 41) ++ processedNew.slice(30, 35)).reverse)
          }
        }
        newState.txTypeAccountTransactions(TransactionType.LeaseCancelTransaction, r2, 10, 5) match {
          case (c, txsWithHeight) => {
            val txs = txsWithHeight.toSeq.map{_._2}
            Seq(c, txs) shouldBe Seq(20, (processedPre.slice(56, 61) ++ processedNew.slice(50, 55)).reverse)
          }
        }
      }
    }
  }
}
