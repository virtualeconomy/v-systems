package vsys.events

import vsys.settings._
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import net.ceedubs.ficus.Ficus._
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}
import com.typesafe.config.{ConfigFactory, Config}
import vsys.blockchain.transaction.{TransactionStatus, ProcessedTransaction, MintingTransaction, PaymentTransaction}
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.state.BlockDiff
import vsys.blockchain.state.ByteStr
import vsys.blockchain.state.Diff
import vsys.blockchain.state._
import vsys.account.Address
import vsys.settings.WebhookEventSettings


class EventTriggersSpec extends FlatSpec with Matchers with PrivateMethodTester with MockitoSugar {

  "Private Method checkRules" should "filter AfterHeight correctly" in {
    val eventRules = Seq(AfterHeight(12))
    val blockDiff_1 = createBlockDiff(0)
    val blockDiff_2 = createBlockDiff(15)

    EventTrigger.checkRules(eventRules, blockDiff_1) shouldBe(List.empty)
    EventTrigger.checkRules(eventRules, blockDiff_2).size shouldBe(6)
  }

  it should "filter AfterTime correctly" in {
    val defaultTime = Seq(AfterTime(0))
    val custTime = Seq(AfterTime(100))
    val blockDiff = createBlockDiff(0)

    EventTrigger.checkRules(defaultTime, blockDiff).size shouldBe(6)
    EventTrigger.checkRules(custTime, blockDiff)(0)._2.transaction.timestamp shouldBe(100)
  }

  it should "filter WithTxs correctly" in {
    val defaultRule = Seq(WithTxs(false))
    val custRule = Seq(WithTxs(true))
    val blockDiff = createBlockDiff(0)

    EventTrigger.checkRules(defaultRule, blockDiff) shouldBe(List.empty)
    EventTrigger.checkRules(custRule, blockDiff).size shouldBe(6)
    // EventTrigger.checkRules(defaultRule, mintTxs, 0) shouldBe(Seq.empty)
    // EventTrigger.checkRules(custRule, mintTxs, 0) shouldBe(mintTxs)
  }

  it should "filter WithMintingTxs correctly" in {
    val defaultRule = Seq(WithMintingTxs(false))
    val custRule = Seq(WithMintingTxs(true))
    val blockDiff = createBlockDiff(0)

    EventTrigger.checkRules(defaultRule, blockDiff).size shouldBe(1)
    EventTrigger.checkRules(custRule, blockDiff).size shouldBe(6)
  }

  "EventTrigger" should "return transaction correctly in evoke" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
         | Event {
         |   webHooks: [
         |     {
         |       events: [
         |         {
         |           type: 1,
         |           rules: {
         |             afterHeight: 10,
         |             afterTime: 15,
         |             withTxs: true,
         |             withMintingTxs: false
         |           }
         |         }
         |       ]
         |     }
         |   ]
         | }
         |}""".stripMargin))
    val hookConf = config.as[Seq[Config]]("vsys.Event.webHooks")
    val webhookEventConf = hookConf(0).as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "1"))
    val blockDiff = createBlockDiff(12)
    
    EventTrigger.evoke(webhookEventConf(0).explicitGet(), blockDiff)

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