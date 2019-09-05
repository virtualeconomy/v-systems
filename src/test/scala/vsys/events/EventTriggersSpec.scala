package vsys.events

import vsys.account.Address
import vsys.settings._
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}
import vsys.blockchain.transaction.{TransactionStatus, ProcessedTransaction, MintingTransaction}

class EventTriggersSpec extends FlatSpec with Matchers with PrivateMethodTester with MockitoSugar {

  "Private Method checkRules" should "filter AfterHeight correctly" in {
    val eventRules = Seq(AfterHeight(12))
    val mintTxs = (1 to 5).map(createTxs(_))

    EventTrigger.checkRules(eventRules, mintTxs, 0) shouldBe(Seq.empty)
    EventTrigger.checkRules(eventRules, mintTxs, 12) shouldBe(mintTxs)
  }

  "private Method checkRules" should "filter AfterTime correctly" in {
    val defaultTime = Seq(AfterTime(0))
    val custTime = Seq(AfterTime(50))
    val mintTxs = (1 to 5).map(createTxs(_))

    EventTrigger.checkRules(defaultTime, mintTxs, 0) shouldBe(mintTxs)
    EventTrigger.checkRules(custTime, mintTxs, 0) shouldBe(Seq.empty)
  }


  private def createTxs(i: Int): ProcessedTransaction = {
    val tx = mock[MintingTransaction]
    when(tx.amount).thenReturn(i * 1000)
    when(tx.timestamp).thenReturn(i * 10)
    ProcessedTransaction(TransactionStatus.Success, 1, tx)
  }
}