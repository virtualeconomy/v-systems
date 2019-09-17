package vsys.settings

import com.typesafe.config.{ConfigFactory, Config}
import org.scalatest.{FlatSpec, Matchers}
import net.ceedubs.ficus.Ficus._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import vsys.blockchain.transaction.{ProcessedTransaction, MintingTransaction, PaymentTransaction}
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.account.Address

class WebhookEventRulesSpec extends FlatSpec with Matchers with MockitoSugar {

  "WebhookEventRules" should "parse config value correctly" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
      |  Event {
      |    rules: {
      |      afterHeight: 1,
      |      afterTime: 2,
      |      withTxs: true,
      |      withMintingTxs: false,
      |      relatedAccount: ["addr3", "addr4"],
      |      includeTypes: [1, 2],
      |      excludeTypes: [3, 4],
      |
      |      amount: {
      |        gt: 10,
      |        gte: 20,
      |        lt: 30,
      |        lte: 40,
      |        withFee: false
      |      },
      |
      |      withTxsOfTypes: [1, 2],
      |      withTxsOfAccs: ["addr3", "addr4"],
      |      withStateOfAccs: ["addr5", "addr6"]
      |    }
      |  }
      |}""".stripMargin)).as[Config]("vsys.Event")

    AfterHeight.fromConfig(config) shouldBe(Some(AfterHeight(1)))
    AfterTime.fromConfig(config) shouldBe(Some(AfterTime(2)))
    WithTxs.fromConfig(config) shouldBe(Some(WithTxs(true)))
    WithMintingTxs.fromConfig(config) shouldBe(Some(WithMintingTxs(false)))
    RelatedAccs.fromConfig(config) shouldBe(Some(RelatedAccs(Seq("addr3", "addr4"))))
    IncludeTypes.fromConfig(config) shouldBe(Some(IncludeTypes(Seq(1, 2))))
    ExcludeTypes.fromConfig(config) shouldBe(Some(ExcludeTypes(Seq(3, 4))))
    Amount.fromConfig(config) shouldBe(Some(Amount(Seq(AmtGT(10), AmtGTE(20), AmtLT(30), AmtLTE(40), AmtWithFee(false)))))
    WithTxsOfTypes.fromConfig(config) shouldBe(Some(WithTxsOfTypes(Seq(1, 2))))
    WithTxsOfAccs.fromConfig(config) shouldBe(Some(WithTxsOfAccs(Seq("addr3", "addr4"))))
    WithStateOfAccs.fromConfig(config) shouldBe(Some(WithStateOfAccs(Seq("addr5", "addr6"))))
  }

  it should "get expected value from applyRule" in {
    val mockTx1 = mock[ProcessedTransaction]
    val mockTx2 = mock[ProcessedTransaction]

    val tx = mock[MintingTransaction]
    when(tx.timestamp).thenReturn(10)
    when(tx.amount).thenReturn(200)
    when(tx.transactionType).thenReturn(TransactionType.MintingTransaction)
    when(mockTx1.transaction).thenReturn(tx)

    val tx2 = mock[PaymentTransaction]
    when(tx2.timestamp).thenReturn(5)
    when(tx2.transactionFee).thenReturn(100)
    when(tx2.amount).thenReturn(150)
    when(tx2.transactionType).thenReturn(TransactionType.PaymentTransaction)
    when(mockTx2.transaction).thenReturn(tx2)

    AfterHeight(10).applyRule(10, mockTx1, Set.empty) shouldBe(true)
    AfterHeight(10).applyRule(3, mockTx1, Set.empty) shouldBe(false)

    AfterTime(10).applyRule(0, mockTx1, Set.empty) shouldBe(true)
    AfterTime(10).applyRule(0, mockTx2, Set.empty) shouldBe(false)

    WithTxs(true).applyRule(0, mockTx1, Set.empty) shouldBe(true)

    WithMintingTxs(true).applyRule(0, mockTx1, Set.empty) shouldBe(true)
    WithMintingTxs(true).applyRule(0, mockTx2, Set.empty) shouldBe(true)
    WithMintingTxs(false).applyRule(0, mockTx1, Set.empty) shouldBe(false)

    val addr1 = mock[Address]
    val addr2 = mock[Address]
    when(addr1.toString).thenReturn("addr1")
    when(addr2.toString).thenReturn("addr2")
    RelatedAccs(Seq("addr1")).applyRule(0, mockTx1, Set(addr1, addr2)) shouldBe(true)
    RelatedAccs(Seq("addr1")).applyRule(0, mockTx1, Set(addr2)) shouldBe(false)
    RelatedAccs(Seq("addr1")).applyRule(0, mockTx1, Set.empty) shouldBe(false)
    RelatedAccs(Seq.empty).applyRule(0, mockTx1, Set.empty) shouldBe(true)

    IncludeTypes(Seq(1)).applyRule(0, mockTx1, Set.empty) shouldBe(false)
    IncludeTypes(Seq(1, 2)).applyRule(0, mockTx2, Set.empty) shouldBe(true)
    IncludeTypes(Seq.empty).applyRule(0, mockTx1, Set.empty) shouldBe(false)

    ExcludeTypes(Seq(1)).applyRule(0, mockTx1, Set.empty) shouldBe(true)
    ExcludeTypes(Seq(1, 2)).applyRule(0, mockTx2, Set.empty) shouldBe(false)
    ExcludeTypes(Seq.empty).applyRule(0, mockTx1, Set.empty) shouldBe(true)

    Amount(Seq(AmtWithFee(true))).applyRule(0, mockTx1, Set.empty) shouldBe(true)
    Amount(Seq(AmtWithFee(false))).applyRule(0, mockTx1, Set.empty) shouldBe(true)
    Amount(Seq.empty).applyRule(0, mockTx1, Set.empty) shouldBe(true)

    Amount(Seq(AmtWithFee(true), AmtGT(250))).applyRule(0, mockTx2, Set.empty) shouldBe(false)
    Amount(Seq(AmtWithFee(true), AmtGT(249))).applyRule(0, mockTx2, Set.empty) shouldBe(true)
    Amount(Seq(AmtGT(150))).applyRule(0, mockTx2, Set.empty) shouldBe(false)
    Amount(Seq(AmtGT(149))).applyRule(0, mockTx2, Set.empty) shouldBe(true)

    Amount(Seq(AmtGTE(150))).applyRule(0, mockTx2, Set.empty) shouldBe(true)
    Amount(Seq(AmtGTE(151))).applyRule(0, mockTx2, Set.empty) shouldBe(false)
    Amount(Seq(AmtWithFee(true), AmtGTE(250))).applyRule(0, mockTx2, Set.empty) shouldBe(true)
    Amount(Seq(AmtWithFee(true), AmtGTE(251))).applyRule(0, mockTx2, Set.empty) shouldBe(false)

    Amount(Seq(AmtLT(151))).applyRule(0, mockTx2, Set.empty) shouldBe(true)
    Amount(Seq(AmtLT(150))).applyRule(0, mockTx2, Set.empty) shouldBe(false)

    Amount(Seq(AmtLTE(150))).applyRule(0, mockTx2, Set.empty) shouldBe(true)
    Amount(Seq(AmtLTE(149))).applyRule(0, mockTx2, Set.empty) shouldBe(false)
  }

  it should "get default value" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        Event {
          rules: {}
        }
      }""".stripMargin)).as[Config]("vsys.Event")

    AfterHeight.fromConfig(config) shouldBe(None)
    AfterTime.fromConfig(config) shouldBe(None)
    WithTxs.fromConfig(config) shouldBe(None)
    WithMintingTxs.fromConfig(config) shouldBe(None)
    RelatedAccs.fromConfig(config) shouldBe(None)
    IncludeTypes.fromConfig(config) shouldBe(None)
    ExcludeTypes.fromConfig(config) shouldBe(None)
    Amount.fromConfig(config) shouldBe(None)
    WithTxsOfTypes.fromConfig(config) shouldBe(None)
    WithTxsOfAccs.fromConfig(config) shouldBe(None)
    WithStateOfAccs.fromConfig(config) shouldBe(None)
  }
}