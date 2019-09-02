package vsys.settings

import com.typesafe.config.{ConfigFactory, Config}
import org.scalatest.{FlatSpec, Matchers}
import net.ceedubs.ficus.Ficus._

class WebhookEventRulesSpec extends FlatSpec with Matchers {

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
    AmtGT.fromConfig(config) shouldBe(Some(AmtGT(10)))
    AmtGTE.fromConfig(config) shouldBe(Some(AmtGTE(20)))
    AmtLT.fromConfig(config) shouldBe(Some(AmtLT(30)))
    AmtLTE.fromConfig(config) shouldBe(Some(AmtLTE(40)))
    AmtWithFee.fromConfig(config) shouldBe(Some(AmtWithFee(false)))
    WithTxsOfTypes.fromConfig(config) shouldBe(Some(WithTxsOfTypes(Seq(1, 2))))
    WithTxsOfAccs.fromConfig(config) shouldBe(Some(WithTxsOfAccs(Seq("addr3", "addr4"))))
    WithStateOfAccs.fromConfig(config) shouldBe(Some(WithStateOfAccs(Seq("addr5", "addr6"))))
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
    AmtGT.fromConfig(config) shouldBe(None)
    AmtGTE.fromConfig(config) shouldBe(None)
    AmtLT.fromConfig(config) shouldBe(None)
    AmtLTE.fromConfig(config) shouldBe(None)
    AmtWithFee.fromConfig(config) shouldBe(None)
    WithTxsOfTypes.fromConfig(config) shouldBe(None)
    WithTxsOfAccs.fromConfig(config) shouldBe(None)
    WithStateOfAccs.fromConfig(config) shouldBe(None)
  }
}