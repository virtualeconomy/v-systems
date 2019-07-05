package com.wavesplatform

import java.util.concurrent.locks.ReentrantReadWriteLock

import vsys.blockchain.history.{HistoryWriterImpl, StorageFactory}
import vsys.settings.{BlockchainSettings, FeeSettings, FeesSettings, FunctionalitySettings, UtxSettings}
import org.scalacheck.Gen._
import org.scalacheck.{Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}
import vsys.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import vsys.blockchain.block.Block
import vsys.blockchain.transaction.{FeeCalculator, PaymentTransaction, Transaction}
import vsys.utils.Time
import scala.concurrent.duration._

import vsys.blockchain.consensus.SPoSCalc._
import vsys.db.openDB
import vsys.blockchain.transaction.MintingTransaction
import vsys.settings.TestStateSettings

class UtxPoolSpecification extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  private val db = openDB("./test/utx/data", true)

  private val calculator = new FeeCalculator(FeesSettings(Map(
    1 -> List(FeeSettings("", 0)),
    2 -> List(FeeSettings("", 0)),
    4 -> List(FeeSettings("", 0))
  )))

  private def mkState(senderAccount: Address, senderBalance: Long) = {
    val genesisSettings = TestHelpers.genesisSettings(Map(senderAccount -> senderBalance))
    val (history, _, state, bcu) =
      StorageFactory(db, BlockchainSettings('T', 5, FunctionalitySettings.TESTNET, genesisSettings, TestStateSettings.AllOn), true)

    bcu.processBlock(Block.genesis(genesisSettings).right.get)

    (state, history)
  }

  /* this function is replaced by payment(). The reason is that we disabled transfer transaction
   * for the first release so this test will fail.
   */
  //private def transfer(sender: PrivateKeyAccount, maxAmount: Long, time: Time) = (for {
  //  amount <- chooseNum(1, (maxAmount * 0.9).toLong)
  //  recipient <- accountGen
  //  fee <- chooseNum(1, (maxAmount * 0.1).toLong)
  //} yield TransferTransaction.create(None, sender, recipient, amount, time.getTimestamp(), None, fee, Array.empty[Byte]).right.get)
  //  .label("transferTransaction")

  private def payment(sender: PrivateKeyAccount, maxAmount: Long, time: Time) = (for {
    amount <- chooseNum(1, (maxAmount * 0.9).toLong)
    recipient <- accountGen
    fee <- chooseNum(1, (maxAmount * 0.1).toLong)
    attachment <- genBoundedBytes(0, PaymentTransaction.MaxAttachmentSize)
    // feeScale <- chooseNum(1, 100)
  } yield PaymentTransaction.create(sender, recipient, amount, fee, 100, time.getTimestamp(), attachment).right.get)
    .label("paymentTransaction")

  /* this function is replaced by paymentWithRecipent(). The reason is that we disabled transfer transaction
   * for the first release so this test will fail.
   */
  //private def transferWithRecipient(sender: PrivateKeyAccount, recipient: PublicKeyAccount, maxAmount: Long, time: Time) = (for {
  //  amount <- chooseNum(1, (maxAmount * 0.9).toLong)
  //  fee <- chooseNum(1, (maxAmount * 0.1).toLong)
  //} yield TransferTransaction.create(None, sender, recipient, amount, time.getTimestamp(), None, fee, Array.empty[Byte]).right.get)
  //  .label("transferWithRecipient")

  private def paymentWithRecipient(sender: PrivateKeyAccount, recipient: PublicKeyAccount, maxAmount: Long, time: Time) = (for {
    amount <- chooseNum(1, (maxAmount * 0.9).toLong)
    fee <- chooseNum(1, (maxAmount * 0.1).toLong)
    // feeScale <- chooseNum(1, 100)
    attachment <- genBoundedBytes(0, PaymentTransaction.MaxAttachmentSize)
  } yield PaymentTransaction.create(sender, recipient, amount, fee, 100, time.getTimestamp(), attachment).right.get)
    .label("paymentWithRecipient")

  private def mintingTransaction(sender: PrivateKeyAccount, amount: Long, time: Time, height: Int) = {
    MintingTransaction.create(sender, amount, time.getTimestamp(), height).right.get
  }

  private val stateGen = for {
    sender <- accountGen.label("sender")
    senderBalance <- positiveLongGen.label("senderBalance")
  } yield {
    val (state, history) = mkState(sender, senderBalance)
    (sender, senderBalance, state, history)
  }
  private val twoOutOfManyValidPayments = (for {
    (sender, senderBalance, state, history) <- stateGen
    recipient <- accountGen
    n <- chooseNum(3, 10)
    fee <- chooseNum(1, (senderBalance * 0.01).toLong)
    //feeScale <- chooseNum(1, 100)
    offset <- chooseNum(1000000000L, 2000000000L)
  } yield {
    val time = new TestTime()
    val utx = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, UtxSettings(10, 10.minutes))
    val amountPart = (senderBalance - fee) / 2 - fee
    val attachment: Array[Byte] = Array()
    val txs = for (_ <- 1 to n) yield PaymentTransaction.create(sender, recipient, amountPart, fee, 100, time.getTimestamp(), attachment).right.get
    (utx, time, txs, (offset + 1000000000L).nanos)
  }).label("twoOutOfManyValidPayments")

  private val emptyUtxPool = stateGen
    .map { case (sender, senderBalance, state, history) =>
      val time = new TestTime()
      val utxPool = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, UtxSettings(10, 1.minute))
      (sender, state, utxPool)
    }
    .label("emptyUtxPool")

  private val withValidPayments = (for {
    (sender, senderBalance, state, history) <- stateGen
    recipient <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(paymentWithRecipient(sender, recipient, senderBalance / 10, time))
  } yield {
    val settings = UtxSettings(10, 1.minute)
    val utxPool = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, settings)
    txs.foreach(utxPool.putIfNew)
    (sender, state, utxPool, time, settings)
  }).label("withValidPayments")

  private val withMintingTransaction = (for {
    (sender, senderBalance, state, history) <- stateGen
    time = new TestTime()
    tx = mintingTransaction(sender, MintingReward, time, history.height())
  } yield {
    val settings = UtxSettings(10, 1.minute)
    val utxPool = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, settings)
    val result = utxPool.putIfNew(tx.asInstanceOf[Transaction])
    (sender, state, utxPool, result)
  }).label("withMintingTransaction")

  private def utxTest(utxSettings: UtxSettings = UtxSettings(20, 5.seconds), txCount: Int = 10)
                     (f: (Seq[PaymentTransaction], UtxPool, TestTime) => Unit): Unit = forAll(
    stateGen,
    chooseNum(2, txCount).label("txCount")) { case ((sender, senderBalance, state, history), count) =>
    val time = new TestTime()
    forAll(listOfN(count, payment(sender, senderBalance / 2, time))) { txs =>
      val utx = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, utxSettings)
      f(txs, utx, time)
    }
  }

  private val dualTxGen: Gen[(UtxPool, TestTime, Seq[Transaction], FiniteDuration, Seq[Transaction])] =
    for {
      (sender, senderBalance, state, history) <- stateGen
      ts = System.currentTimeMillis()*1000000L+System.nanoTime()%1000000L
      count1 <- chooseNum(5, 10)
      tx1 <- listOfN(count1, payment(sender, senderBalance / 2, new TestTime(ts)))
      offset <- chooseNum(5000000000L, 10000000000L)
      tx2 <- listOfN(count1, payment(sender, senderBalance / 2, new TestTime(ts + offset + 1000000000L)))
    } yield {
      val time = new TestTime()
      val history = new HistoryWriterImpl(db, new ReentrantReadWriteLock(), true)
      val utx = new UtxPool(time, state, history, calculator, FunctionalitySettings.TESTNET, UtxSettings(10, offset.nanos))
      (utx, time, tx1, (offset + 1000000000L).nanos, tx2)
    }

  "UTX Pool" - {
    "does not add new transactions when full" in utxTest(UtxSettings(1, 5.seconds)) { (txs, utx, _) =>
      utx.putIfNew(txs.head) shouldBe 'right
      all(txs.tail.map(t => utx.putIfNew(t))) shouldBe 'left
    }

    "does not broadcast the same transaction twice" in utxTest() { (txs, utx, _) =>
      utx.putIfNew(txs.head) shouldBe 'right
      utx.putIfNew(txs.head) shouldBe 'right
    }

    "evicts expired transactions when removeAll is called" in forAll(dualTxGen) { case (utx, time, txs1, offset, txs2) =>
      all(txs1.map { t =>
        utx.putIfNew(t)
      }) shouldBe 'right
      utx.all().size shouldEqual txs1.size

      time.advance(offset)
      utx.removeAll(Seq.empty)

      all(txs2.map { t =>
        utx.putIfNew(t)
      }) shouldBe 'right
      utx.all().size shouldEqual txs2.size
    }

    "evicts expired transactions when packUnconfirmed is called" in forAll(dualTxGen) { case (utx, time, txs, offset, _) =>
      all(txs.map { t =>
        utx.putIfNew(t)
      }) shouldBe 'right
      utx.all().size shouldEqual txs.size

      time.advance(offset)

      utx.packUnconfirmed() shouldBe 'empty
      utx.all() shouldBe 'empty
    }

    "evicts one of mutually invalid transactions when packUnconfirmed is called" in forAll(twoOutOfManyValidPayments) { case (utx, time, txs, offset) =>
      all(txs.map { t =>
        utx.putIfNew(t)
      }) shouldBe 'right
      utx.all().size shouldEqual txs.size

      time.advance(offset)

      utx.packUnconfirmed().size shouldBe 2
      utx.all().size shouldBe 2
    }

    "portfolio" - {
      "returns a count of assets from the state if there is no transaction" in forAll(emptyUtxPool) { case (sender, state, utxPool) =>
        val basePortfolio = state.accountPortfolio(sender)

        utxPool.size shouldBe 0
        val utxPortfolio = utxPool.portfolio(sender)

        basePortfolio shouldBe utxPortfolio
      }

      "taking into account unconfirmed transactions" in forAll(withValidPayments) { case (sender, state, utxPool, _, _) =>
        val basePortfolio = state.accountPortfolio(sender)

        utxPool.size should be > 0
        val utxPortfolio = utxPool.portfolio(sender)

        utxPortfolio.balance should be <= basePortfolio.balance
        utxPortfolio.leaseInfo.leaseOut should be <= basePortfolio.leaseInfo.leaseOut
        // should not be changed
        utxPortfolio.leaseInfo.leaseIn shouldBe basePortfolio.leaseInfo.leaseIn
        utxPortfolio.assets.foreach { case (assetId, count) =>
          count should be <= basePortfolio.assets.getOrElse(assetId, count)
        }
      }

      "is changed after transactions with these assets are removed" in forAll(withValidPayments) { case (sender, _, utxPool, time, settings) =>
        val utxPortfolioBefore = utxPool.portfolio(sender)
        val poolSizeBefore = utxPool.size

        time.advance(settings.maxTransactionAge * 2)
        utxPool.packUnconfirmed()

        poolSizeBefore should be > utxPool.size
        val utxPortfolioAfter = utxPool.portfolio(sender)

        utxPortfolioAfter.balance should be >= utxPortfolioBefore.balance
        utxPortfolioAfter.leaseInfo.leaseOut should be >= utxPortfolioBefore.leaseInfo.leaseOut
        utxPortfolioAfter.assets.foreach { case (assetId, count) =>
          count should be >= utxPortfolioBefore.assets.getOrElse(assetId, count)
        }
      }
    }

    "ignore minting transaction" in forAll(withMintingTransaction) { case (sender, state, utxPool, result) =>
      utxPool.size shouldEqual 0
      result.toString should include ("Cannot add MintingTransaction to transaction pool")
    }
  }
}
