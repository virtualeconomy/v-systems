package vsys.events

import akka.actor.{ActorSystem, Props}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.scalatest.{FlatSpec, Matchers}
import vsys.blockchain.transaction.{TransactionStatus, ProcessedTransaction, MintingTransaction, PaymentTransaction}
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.state.{BlockDiff, Diff, ByteStr}
import vsys.blockchain.state._
import vsys.account.Address
import vsys.settings.{AfterHeight, AfterTime, WithTxs, WithMintingTxs}
import vsys.settings.EventSettings

class EventTriggersSpec extends FlatSpec with Matchers with MockitoSugar {
  val system = ActorSystem()
  val trigger = EventTrigger(system.actorOf(Props[EventWriterActor], name = "testing"), EventSettings(Seq.empty))

  "Private Method checkRules" should "filter AfterHeight correctly" in {
    val eventRules = Seq(AfterHeight(12))
    val blockDiff1 = createBlockDiff(0)
    val blockDiff2 = createBlockDiff(15)

    trigger.checkRules(eventRules, blockDiff1) shouldBe(List.empty)
    trigger.checkRules(eventRules, blockDiff2).size shouldBe(6)
  }

  it should "filter AfterTime correctly" in {
    val defaultTime = Seq(AfterTime(0))
    val custTime = Seq(AfterTime(100))
    val blockDiff = createBlockDiff(0)

    trigger.checkRules(defaultTime, blockDiff).size shouldBe(6)
    trigger.checkRules(custTime, blockDiff).collectFirst {case (_, p: ProcessedTransaction, _) =>
      p.transaction.timestamp shouldBe(100)
    }
  }

  it should "filter WithTxs correctly" in {
    val defaultRule = Seq(WithTxs(false))
    val custRule = Seq(WithTxs(true))
    val blockDiff = createBlockDiff(0)

    trigger.checkRules(defaultRule, blockDiff) shouldBe(List.empty)
    trigger.checkRules(custRule, blockDiff).size shouldBe(6)
  }

  it should "filter WithMintingTxs correctly" in {
    val defaultRule = Seq(WithMintingTxs(false))
    val custRule = Seq(WithMintingTxs(true))
    val blockDiff = createBlockDiff(0)

    trigger.checkRules(defaultRule, blockDiff).size shouldBe(1)
    trigger.checkRules(custRule, blockDiff).size shouldBe(6)
  }

  private def createTxs(i: Int): ProcessedTransaction = {
    val tx = mock[MintingTransaction]
    val ptx = mock[ProcessedTransaction]

    when(tx.amount).thenReturn(i * 1000)
    when(tx.timestamp).thenReturn(i * 10)
    when(tx.transactionType).thenReturn(TransactionType.MintingTransaction)
    when(ptx.status).thenReturn(TransactionStatus.Success)
    when(ptx.feeCharged).thenReturn(1)
    when(ptx.transaction).thenReturn(tx)
    ptx
  }

  private def createBlockDiff(height: Int): BlockDiff = {
    val blockDiff = mock[BlockDiff]
    val txsDiff = mock[Diff]
    val mintTxs = (1 to 5).map(createTxs(_))

    val payTx = mock[PaymentTransaction]
    when(payTx.transactionType).thenReturn(TransactionType.PaymentTransaction)
    when(payTx.timestamp).thenReturn(100)
    val payTxs = ProcessedTransaction(TransactionStatus.Success, 1, payTx)

    val txsResp: Map[ByteStr, (Int, ProcessedTransaction, Set[Address])] = Map(
      (ByteStr(Array(1.toByte)), (height, mintTxs(0), Set.empty)),
      (ByteStr(Array(2.toByte)), (height, mintTxs(1), Set.empty)),
      (ByteStr(Array(3.toByte)), (height, mintTxs(2), Set.empty)),
      (ByteStr(Array(4.toByte)), (height, mintTxs(3), Set.empty)),
      (ByteStr(Array(5.toByte)), (height, mintTxs(4), Set.empty)),
      (ByteStr(Array(6.toByte)), (height, payTxs, Set.empty))
    )

    when(txsDiff.transactions).thenReturn(txsResp)
    when(blockDiff.txsDiff).thenReturn(txsDiff)
    blockDiff
  }
}