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

    

    AfterHeight.fromConfig(config).get.value shouldBe(1)
    AfterTime.fromConfig(config).get.value shouldBe(2)
    WithTxs.fromConfig(config).get.value shouldBe(true)    
    WithMintingTxs.fromConfig(config).get.value shouldBe(false)
    RelatedAccs.fromConfig(config).get.value shouldBe(Seq("addr3", "addr4"))
    IncludeTypes.fromConfig(config).get.value shouldBe(Seq(1, 2))
    ExcludeTypes.fromConfig(config).get.value shouldBe(Seq(3, 4))
    AmtGT.fromConfig(config).get.value shouldBe(10)
    AmtGTE.fromConfig(config).get.value shouldBe(20)
    AmtLT.fromConfig(config).get.value shouldBe(30)
    AmtLTE.fromConfig(config).get.value shouldBe(40)
    AmtWithFee.fromConfig(config).get.value shouldBe(false)
    WithTxsOfTypes.fromConfig(config).get.value shouldBe(Seq(1, 2))
    WithTxsOfAccs.fromConfig(config).get.value shouldBe(Seq("addr3", "addr4"))
    WithStateOfAccs.fromConfig(config).get.value shouldBe(Seq("addr5", "addr6"))

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