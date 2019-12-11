package vsys.events

import akka.actor.{ActorSystem, Props}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.{PropertyChecks, GeneratorDrivenPropertyChecks}
import vsys.blockchain.transaction.{TransactionStatus, ProcessedTransaction, MintingTransaction, PaymentTransaction}
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.state._
import vsys.account.Address
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction
import vsys.settings.{EventSettings, AfterHeight, AfterTime, WithTxs, WithMintingTxs}
import vsys.utils.SimpleEventQueue

class EventTriggersSpec extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with MockitoSugar {
  val system = ActorSystem()
  val blockTime = 10
  val eventQueue = new SimpleEventQueue()
  val eventDispatchActor = system.actorOf(Props(EventDispatchActor(system)))
  val trigger = EventTriggers(system.actorOf(Props(EventWriterActor(eventQueue, eventDispatchActor)), name = "testing"), EventSettings(Seq.empty, 0))

  property("Private Method filterTxs should filter AfterHeight correctly") {
    val eventRules = Seq(AfterHeight(12))
    val blockDiff1 = createBlockDiff(0, createMintingTxs)
    val blockDiff2 = createBlockDiff(15, createMintingTxs)

    trigger.filterTxs(eventRules, blockTime, blockDiff1) shouldBe(List.empty)
    trigger.filterTxs(eventRules, blockTime, blockDiff2).size shouldBe(6)
  }

  // it should "filter AfterTime correctly" in {
  //   val defaultTime = Seq(AfterTime(0))
  //   val custTime = Seq(AfterTime(100))
  //   val blockDiff = createBlockDiff(0, createMintingTxs)

  //   trigger.filterTxs(defaultTime, blockTime, blockDiff).size shouldBe(6)
  //   trigger.filterTxs(custTime, blockTime, blockDiff).size shouldBe(0)
  // }

  // it should "filter WithTxs correctly" in {
  //   val defaultRule = Seq(WithTxs(false))
  //   val custRule = Seq(WithTxs(true))
  //   val blockDiff = createBlockDiff(0, createMintingTxs)

  //   trigger.filterTxs(defaultRule, blockTime, blockDiff) shouldBe(List.empty)
  //   trigger.filterTxs(custRule, blockTime, blockDiff).size shouldBe(6)
  // }

  // it should "filter WithMintingTxs correctly" in {
  //   val defaultRule = Seq(WithMintingTxs(false))
  //   val custRule = Seq(WithMintingTxs(true))
  //   val blockDiff = createBlockDiff(0, createMintingTxs)

  //   trigger.filterTxs(defaultRule, blockTime, blockDiff).size shouldBe(1)
  //   trigger.filterTxs(custRule, blockTime, blockDiff).size shouldBe(6)
  // }

  private def createMintingTxs(i: Int): ProcessedTransaction = {
    val tx = mock[MintingTransaction]
    val ptx = mock[ProcessedTransaction]

    when(tx.amount).thenReturn(i * 1000)
    when(tx.transactionType).thenReturn(TransactionType.MintingTransaction)
    when(ptx.status).thenReturn(TransactionStatus.Success)
    when(ptx.feeCharged).thenReturn(1)
    when(ptx.transaction).thenReturn(tx)
    ptx
  }

  private def createExecuteContractTxs(i: Int): ProcessedTransaction = {
    val tx = mock[ExecuteContractFunctionTransaction]
    val ptx = mock[ProcessedTransaction]
    ptx
  }

  private def createBlockDiff(height: Int, create: Int => ProcessedTransaction): BlockDiff = {
    val blockDiff = mock[BlockDiff]
    val txsDiff = mock[Diff]
    val mintTxs = (1 to 5).map(create(_))

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