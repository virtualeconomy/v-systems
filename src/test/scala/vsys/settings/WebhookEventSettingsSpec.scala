package vsys.settings

import com.typesafe.config.{ConfigFactory, Config}
import net.ceedubs.ficus.Ficus._
import org.scalatest.{FlatSpec, Matchers}

class WebhookEventSettingsSpec extends FlatSpec with Matchers {
  "Block Appended Event Settings" should "form Webhook Event Rules Correctly" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
         | Event {
         |   webHooks: [
         |     {
         |       events: [
         |         {
         |           type: 1,
         |           rules: {
         |             afterHeight: 50,
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
    val aHook = hookConf(0)
    val eventList = aHook.as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "1"))

    eventList(0) shouldBe(Right(BlockAppendedEventSettings(
      Seq(WithTxs(true), WithMintingTxs(false), AfterHeight(50), AfterTime(15))
    )))
  }

  it should "only have withTxs and withMintingTxs Rules(default)" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
         | Event {
         |   webHooks: [
         |     {
         |       events: [
         |         {
         |           type: 1,
         |         }
         |       ]
         |     }
         |   ]
         | }
         |}""".stripMargin))

    val hookConf = config.as[Seq[Config]]("vsys.Event.webHooks")
    val aHook = hookConf(0)
    val eventList = aHook.as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "1"))

    eventList(0) shouldBe(Right(BlockAppendedEventSettings(Seq.empty)))
  }

  "Tx Confirmed Event Settings" should "form Webhook Event Rules Correctly" in {
      val config = loadConfig(ConfigFactory.parseString(
      """vsys {
         | Event {
         |   webHooks: [
         |     {
         |       events: [
         |         {
         |           type: 2,
         |           rules: {
         |             afterHeight: 50,
         |             afterTime: 15,
         |             relatedAccount: ["addr2", "addr3"],
         |             includeTypes: [1, 2],
         |             excludeTypes: [3, 4],
         |             amount: {
         |               gt: 0,
         |               gte: 10,
         |               lte: 100,
         |               lt: 1000,
         |               withFee: true
         |             }
         |           }
         |         }
         |       ]
         |     }
         |   ]
         | }
         |}""".stripMargin))

      val hookConf = config.as[Seq[Config]]("vsys.Event.webHooks")
      val aHook = hookConf(0)
      val eventList = aHook.as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "2"))

      eventList(0) shouldBe(Right(TxConfirmedEventSettings(
        Seq(IncludeTypes(Seq(1, 2)), ExcludeTypes(Seq(3, 4)), RelatedAccs(Seq("addr2", "addr3")),  AfterHeight(50), AfterTime(15),
         Amount(Seq(AmtGT(0), AmtGTE(10), AmtLT(1000), AmtLTE(100), AmtWithFee(true))))
      )))
    }

  it should "only have AmtWithFee rule(default)" in {
    val config = loadConfig(ConfigFactory.parseString(
    """vsys {
       | Event {
       |   webHooks: [
       |     {
       |       events: [
       |         {
       |           type: 2,
       |         }
       |       ]
       |     }
       |   ]
       | }
       |}""".stripMargin))

    val hookConf = config.as[Seq[Config]]("vsys.Event.webHooks")
    val aHook = hookConf(0)
    val eventList = aHook.as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "2"))

    eventList(0) shouldBe(Right(TxConfirmedEventSettings(Seq.empty)))
  }

  "State Updated Event Settings" should "form Webhook Event Rules Correctly" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
         | Event {
         |   webHooks: [
         |     {
         |       events: [
         |         {
         |           type: 3,
         |           rules: {
         |             afterHeight: 50,
         |             afterTime: 15,
         |             relatedAccount: ["addr3", "addr4"]
         |           }
         |         }
         |       ]
         |     }
         |   ]
         | }
         |}""".stripMargin))

    val hookConf = config.as[Seq[Config]]("vsys.Event.webHooks")
    val aHook = hookConf(0)
    val eventList = aHook.as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "3"))

    eventList(0) shouldBe(Right(StateUpdatedEventSettings(
      Seq(AfterTime(15), AfterHeight(50), RelatedAccs(Seq("addr3", "addr4")))
    )))
  }

  it should "have no rule(default)" in {
    val config = loadConfig(ConfigFactory.parseString(
    """vsys {
       | Event {
       |   webHooks: [
       |     {
       |       events: [
       |         {
       |           type: 3,
       |         }
       |       ]
       |     }
       |   ]
       | }
       |}""".stripMargin))

    val hookConf = config.as[Seq[Config]]("vsys.Event.webHooks")
    val aHook = hookConf(0)
    val eventList = aHook.as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "3"))

    eventList(0) shouldBe(Right(StateUpdatedEventSettings(Seq.empty)))
  }

  "Block Rollback Event Setting" should "form Webhook Event Rules Correctly" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
         | Event {
         |   webHooks: [
         |     {
         |       events: [
         |         {
         |           type: 4,
         |           rules: {
         |             afterHeight: 50,
         |             afterTime: 15,
         |             relatedAccount: ["addr7", "addr8"]
         |             withTxsOfAccs: ["addr3", "addr4"]
         |             withTxsOfTypes: [1, 2]
         |             withStateOfAccs: ["addr5", "addr6"]
         |           }
         |         }
         |       ]
         |     }
         |   ]
         | }
         |}""".stripMargin))

    val hookConf = config.as[Seq[Config]]("vsys.Event.webHooks")
    val aHook = hookConf(0)
    val eventList = aHook.as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "4"))

    eventList(0) shouldBe(Right(BlockRollbackEventSettings(
      Seq(AfterTime(15), AfterHeight(50), RelatedAccs(Seq("addr7", "addr8")), WithTxsOfTypes(Seq(1, 2)),
         WithTxsOfAccs(Seq("addr3", "addr4")), WithStateOfAccs(Seq("addr5", "addr6")))
    )))
  }

  it should "have no rule(default)" in {
    val config = loadConfig(ConfigFactory.parseString(
    """vsys {
       | Event {
       |   webHooks: [
       |     {
       |       events: [
       |         {
       |           type: 4,
       |         }
       |       ]
       |     }
       |   ]
       | }
       |}""".stripMargin))

    val hookConf = config.as[Seq[Config]]("vsys.Event.webHooks")
    val aHook = hookConf(0)
    val eventList = aHook.as[Seq[Config]]("events").map(conf => WebhookEventSettings(conf, "4"))

    eventList(0) shouldBe(Right(BlockRollbackEventSettings(Seq.empty)))
  }
}