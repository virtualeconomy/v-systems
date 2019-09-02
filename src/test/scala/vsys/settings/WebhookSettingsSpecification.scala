package vsys.settings

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class WebhookSettingsSpecification extends FlatSpec with Matchers {
  "WebhookSettings" should "read Event values" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
      | # Webhook Event Settings
      |  Event {
      |     enable = true
      |     webHooks: [
      |       {
      |         url: "example.com",
      |         secret: "secret_key",
      |         encryptKey: "pubkey_for_encrypt",
      |         events: [
      |           {
      |             type: 1, # Block Appended
      |             rules: {
      |               withTxs: false,
      |               withMintingTxs: true
      |               afterHeight: 1,
      |               afterTime: 2
      |             }
      |           },
      |
      |           {
      |             type: 2, # Tx Confirmed
      |             rules: {
      |               relatedAccount: [
      |                 "addr1"
      |               ],
      |               afterHeight: 3,
      |               afterTime: 4,
      |
      |               includeTypes: [
      |                 1,
      |                 2
      |               ],
      |
      |               excludeTypes: [
      |                 3,
      |                 4
      |               ],
      |
      |               amount: {
      |                 gte: 0,
      |                 gt: 200,
      |                 lte: 600000000000000000,
      |                 lt: 600000000000000000,
      |                 withFee: true
      |               }
      |             }
      |           },
      |
      |           {
      |             type: 3, # State Updated
      |             rules: {
      |               afterHeight: 5,
      |               afterTime: 6,
      |
      |               relatedAccount: [
      |                 "addr7",
      |                 "addr8"
      |               ]
      |             },
      |           },
      |
      |           {
      |             type: 4, # Block Rollback
      |             rules: {
      |               afterHeight: 7,
      |               afterTime: 8,
      |
      |               relatedAccount: [],
      |
      |               withTxsOfTypes: [
      |                 1,
      |                 2
      |               ],
      |
      |               withTxsOfAccs: [
      |                 "addr3"
      |               ],
      |
      |               withStateOfAccs: [
      |                 "addr4"
      |               ]
      |             }
      |           }
      |
      |         ],
      |         maxSize: 1000
      |       }
      |     ]
      |   }
      | }""".stripMargin))
    val settings = WebhookSettings.fromConfig(config)(0)

    settings.url shouldBe("example.com")
    settings.secretKey shouldBe(Some("secret_key"))
    settings.encryptKey shouldBe(Some("pubkey_for_encrypt"))
    settings.maxSize shouldBe(Some(1000))

    settings.events.length shouldBe(4)
    settings.events map {aEvent =>

      aEvent match {
      case e: BlockAppendedEventSettings =>
        e.typeId shouldBe(1)
        e.typeDescription shouldBe("Block Appended")

        e.eventRules.length shouldBe(4)
        e.eventRules.map(aRule =>
          aRule match {
            case r: AfterHeight => r.value shouldBe(1)
            case r: AfterTime => r.value shouldBe(2)
            case r: WithTxs => r.value shouldBe(false)
            case r: WithMintingTxs => r.value shouldBe(true)
            case _ => fail("Block Appended Event should not contain other rules")
          }
        )

      case e: TxConfirmedEventSettings =>
        e.typeId shouldBe(2)
        e.typeDescription shouldBe("Tx Confirmed")

        e.eventRules.length shouldBe(10)
        e.eventRules.map(aRule =>
          aRule match {
            case r: AfterHeight => r.value shouldBe(3L)
            case r: AfterTime => r.value shouldBe(4L)
            case r: RelatedAccs => r.value shouldBe(Seq("addr1"))
            case r: IncludeTypes => r.value shouldBe(Seq(1, 2))
            case r: ExcludeTypes => r.value shouldBe(Seq(3, 4))
            case r: AmtGTE => r.value shouldBe(0)
            case r: AmtGT => r.value shouldBe(200)
            case r: AmtLTE => r.value shouldBe(600000000000000000L)
            case r: AmtLT => r.value shouldBe(600000000000000000L)
            case r: AmtWithFee => r.value shouldBe(true)
            case _ => fail("Tx Confirmed Event should not contain other rules")
          }
        )

      case e: StateUpdatedEventSettings =>
        e.typeId shouldBe(3)
        e.typeDescription shouldBe("State Updated")

        e.eventRules.length shouldBe(3)
        e.eventRules.map(aRule =>
          aRule match {
            case r: RelatedAccs => r.value shouldBe(Seq("addr7", "addr8"))
            case r: AfterHeight => r.value shouldBe(5L)
            case r: AfterTime => r.value shouldBe(6L)
            case _ => fail("State Updated Event should not contain other rules")
          }
        )

      case e: BlockRollbackEventSettings =>
        e.typeId shouldBe(4)
        e.typeDescription shouldBe("Block Rollback")

        e.eventRules.length shouldBe(6)
        e.eventRules.map(aRule =>
          aRule match {
            case r: RelatedAccs => r.value shouldBe(Seq.empty)
            case r: AfterHeight => r.value shouldBe(7L)
            case r: AfterTime => r.value shouldBe(8L)
            case r: WithTxsOfTypes => r.value shouldBe(Seq(1, 2))
            case r: WithTxsOfAccs => r.value shouldBe(Seq("addr3"))
            case r: WithStateOfAccs => r.value shouldBe(Seq("addr4"))
            case _ => fail("Block Rollback Event should not contain other rules")
          }
        )

      case _ => fail("The filter func in fromConfig is not working correctly")

      }
    }
  }

  it should "read event type in string" in {
    val stringTypeConfig =  loadConfig(ConfigFactory.parseString(
      """vsys {
      | # Webhook Event Settings
      |  Event {
      |     enable = true
      |     webHooks: [
      |       {
      |         url: "example.com",
      |         secret: "secret_key",
      |         encryptKey: "pubkey_for_encrypt",
      |         events: [
      |           {
      |             type: Block Appended
      |           },
      |
      |           {
      |             type: Tx Confirmed,
      |           },
      |
      |           {
      |             type: State Updated,
      |           },
      |
      |           {
      |             type: Block Rollback,
      |           },
      |
      |           {
      |             type: 8
      |           }
      |
      |         ],
      |         maxSize: 1000
      |       }
      |     ]
      |   }
      | }""".stripMargin))

    val stringTypeSetting = WebhookSettings.fromConfig(stringTypeConfig)(0)

    stringTypeSetting.events map {aEvent =>
      aEvent match {
        case e: BlockAppendedEventSettings =>
          e.typeId shouldBe(1)
          e.eventRules.length shouldBe(0)

        case e: TxConfirmedEventSettings =>
          e.typeId shouldBe(2)
          e.eventRules.length shouldBe(0)

        case e: StateUpdatedEventSettings =>
          e.typeId shouldBe(3)
          e.eventRules.length shouldBe(0)

        case e: BlockRollbackEventSettings =>
          e.typeId shouldBe(4)
          e.eventRules.length shouldBe(0)

        case _ => fail("The type description matching in setEventSettings goes wrong")
      }
    }
  }

  "WebhookSettings" should "return default Webhook setting when no event provided" in {
    val defaultConfig = loadConfig(ConfigFactory.parseString(
      """vsys {
      | Event {
      |  enable = true
      |
      | }
      |}""".stripMargin))

    val setInst = WebhookSettings.fromConfig(defaultConfig)(0)

    setInst.url shouldBe("")
    setInst.events shouldBe(Seq.empty)
    setInst.secretKey shouldBe(None)
    setInst.encryptKey shouldBe(None)
    setInst.maxSize shouldBe(None)
  }

  it should "return empty webhook setting " in {
    val defaultConfig = loadConfig(ConfigFactory.parseString(
      """vsys {
      | Event {
      |  enable = true
      |  webHooks: [{}]
      | }
      |}""".stripMargin))

    val setInst = WebhookSettings.fromConfig(defaultConfig)(0)

    setInst.url shouldBe("")
    setInst.events shouldBe(Seq.empty)
  }
}