package vsys.settings

import com.typesafe.config.{ConfigFactory, Config}
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import net.ceedubs.ficus.Ficus._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import vsys.blockchain.transaction.{ProcessedTransaction, MintingTransaction, PaymentTransaction, TransactionGen}
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.account.Address
import org.scalacheck.Gen

class WebhookEventRulesSpec extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with MockitoSugar with TransactionGen {

  property("WebhookEventRules should parse config value correctly") {
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
    Amount.fromConfig(config) shouldBe(Some(Amount(10, 20, 30, 40, false)))
    WithTxsOfTypes.fromConfig(config) shouldBe(Some(WithTxsOfTypes(Seq(1, 2))))
    WithTxsOfAccs.fromConfig(config) shouldBe(Some(WithTxsOfAccs(Seq("addr3", "addr4"))))
    WithStateOfAccs.fromConfig(config) shouldBe(Some(WithStateOfAccs(Seq("addr5", "addr6"))))
  }

  property("WebhookEventRules should get expected value from applyRule") {
    val mockTx1 = mock[ProcessedTransaction]
    val mockTx2 = mock[ProcessedTransaction]

    val tx = mock[MintingTransaction]
    val blockTime1 = 10
    when(tx.amount).thenReturn(200)
    when(tx.transactionType).thenReturn(TransactionType.MintingTransaction)
    when(mockTx1.transaction).thenReturn(tx)

    val tx2 = mock[PaymentTransaction]
    val blockTime2 = 5
    when(tx2.transactionFee).thenReturn(100)
    when(tx2.amount).thenReturn(150)
    when(tx2.transactionType).thenReturn(TransactionType.PaymentTransaction)
    when(mockTx2.transaction).thenReturn(tx2)

    val mockRuleFields: Gen[(Long, Long, ProcessedTransaction, Set[Address], Boolean, Int)] = for {
      height <- Gen.choose(1, Long.MaxValue)
      timestamp <- timestampGen
      pTx <- randomProcessedTransactionGen
      tfVal <- Gen.oneOf(true, false)
      randTxType <- Gen.choose(1, 10)
    } yield(height, timestamp, pTx, Set.empty, tfVal, randTxType)

    forAll (mockRuleFields) {case ((height, timestamp, pTx: ProcessedTransaction, aSet: Set[Address], tfVal, randTxType)) =>
      AfterHeight(100).applyRule(height, timestamp, pTx, aSet) shouldBe(height >= 100)
      AfterTime(100).applyRule(height, timestamp, pTx, aSet) shouldBe(timestamp >= 100)
      WithTxs(tfVal).applyRule(height, timestamp, pTx, aSet) shouldBe(tfVal)
      WithMintingTxs(tfVal).applyRule(height, timestamp, pTx, aSet) shouldBe(pTx.transaction.transactionType != TransactionType.MintingTransaction | tfVal)
      IncludeTypes(Seq(randTxType)).applyRule(height, timestamp, pTx, aSet) shouldBe(pTx.transaction.transactionType.txType == randTxType)
      IncludeTypes(Seq.empty).applyRule(height, timestamp, pTx, aSet) shouldBe(false)
      ExcludeTypes(Seq(randTxType)).applyRule(height, timestamp, pTx, aSet) shouldBe(pTx.transaction.transactionType.txType != randTxType)
      ExcludeTypes(Seq.empty).applyRule(height, timestamp, pTx, aSet) shouldBe(true)
    }

    val addr1 = mock[Address]
    val addr2 = mock[Address]
    when(addr1.toString).thenReturn("addr1")
    when(addr2.toString).thenReturn("addr2")
    RelatedAccs(Seq("addr1")).applyRule(0, blockTime1, mockTx1, Set(addr1, addr2)) shouldBe(true)
    RelatedAccs(Seq("addr1")).applyRule(0, blockTime1, mockTx1, Set(addr2)) shouldBe(false)
    RelatedAccs(Seq("addr1")).applyRule(0, blockTime1, mockTx1, Set.empty) shouldBe(false)
    RelatedAccs(Seq.empty).applyRule(0, blockTime1, mockTx1, Set.empty) shouldBe(true)

    for(withFee <- Seq(false, true)) {
      for(x <- -2 to 2) {
        val amt = if (withFee) 250 else 150
        Amount(amt + x, 0, 1000, 1000, withFee).applyRule(0, blockTime2, mockTx2, Set.empty) shouldBe(x < 0)
        Amount(0, amt + x, 1000, 1000, withFee).applyRule(0, blockTime2, mockTx2, Set.empty) shouldBe(x <= 0 )
        Amount(0, 0, amt + x, 1000, withFee).applyRule(0, blockTime2, mockTx2, Set.empty) shouldBe(x > 0)
        Amount(0, 0, 1000, amt + x, withFee).applyRule(0, blockTime2, mockTx2, Set.empty) shouldBe(x >= 0)
      }
    }
  }

  property("Rules should get default value") {
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