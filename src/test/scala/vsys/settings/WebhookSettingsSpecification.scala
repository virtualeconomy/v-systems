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

    settings.url should be("example.com")
    settings.secretKey should be("secret_key")
    settings.encryptKey should be("pubkey_for_encrypt")
    settings.maxSize should be(1000)

    settings.events.length should be(4)
    settings.events map {aEvent =>

      aEvent match {
      case e: BlockAppendedEventSettings =>
        e.typeId should be(1)
        e.typeDescription should be("Block Appended")

        e.eventRules.length should be(4)
        e.eventRules.map(aRule => 
          aRule match {
            case r: AfterHeight => r.value should be(1)
            case r: AfterTime => r.value should be(2)
            case r: WithTxs => r.value should be(false)
            case r: WithMintingTxs => r.value should be(true)
            case _ => fail("Block Appended Event should not contain other rules")
          }
        )
        
      case e: TxConfirmedEventSettings =>
        e.typeId should be(2)
        e.typeDescription should be("Tx Confirmed")
        
        e.eventRules.length should be(10)
        e.eventRules.map(aRule => 
          aRule match {
            case r: AfterHeight => r.value should be(3L)
            case r: AfterTime => r.value should be(4L)
            case r: RelatedAccs => r.value should be(Seq("addr1"))
            case r: IncludeTypes => r.value should be(Seq(1, 2))
            case r: ExcludeTypes => r.value should be(Seq(3, 4))
            case r: AmtGTE => r.value should be(0)
            case r: AmtGT => r.value should be(200)
            case r: AmtLTE => r.value should be(600000000000000000L)
            case r: AmtLT => r.value should be(600000000000000000L)
            case r: AmtWithFee => r.value should be(true)
            case _ => fail("Tx Confirmed Event should not contain other rules")
          }
        )

      case e: StateUpdatedEventSettings =>
        e.typeId should be(3)
        e.typeDescription should be("State Updated")

        e.eventRules.length should be(3)
        e.eventRules.map(aRule =>
          aRule match {
            case r: RelatedAccs => r.value should be(Seq("addr7", "addr8"))
            case r: AfterHeight => r.value should be(5L)
            case r: AfterTime => r.value should be(6L)
            case _ => fail("State Updated Event should not contain other rules")
          }
        )

      case e: BlockRollbackEventSettings => 
        e.typeId should be(4)
        e.typeDescription should be("Block Rollback")

        e.eventRules.length should be(6)
        e.eventRules.map(aRule => 
          aRule match {
            case r: RelatedAccs => r.value should be(Seq.empty)
            case r: AfterHeight => r.value should be(7L)
            case r: AfterTime => r.value should be(8L)
            case r: WithTxsOfTypes => r.value should be(Seq(1, 2))
            case r: WithTxsOfAccs => r.value should be(Seq("addr3"))
            case r: WithStateOfAccs => r.value should be(Seq("addr4"))
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
      |       {
      |       type: 8
      |           
      |       }
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
          e.typeId should be(1)
          e.eventRules.length shouldBe(0)

        case e: TxConfirmedEventSettings => 
          e.typeId should be(2)
          e.eventRules.length shouldBe(0)

        case e: StateUpdatedEventSettings => 
          e.typeId should be(3)
          e.eventRules.length shouldBe(0)

        case e: BlockRollbackEventSettings => 
          e.typeId should be(4)
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

    setInst.url should be("0.0.0.0")
    setInst.events should be(Seq.empty)
    setInst.secretKey should be("")
    setInst.encryptKey should be("")
    setInst.maxSize should be(1000)
  }
}