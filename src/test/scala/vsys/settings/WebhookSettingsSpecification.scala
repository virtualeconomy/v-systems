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
    |               afterHeight: 0,
    |               afterTime: 0
    |             }
    |           },
    |
    |           {
    |             type: 2, # Tx Confirmed
    |             rules: {
    |               relatedAccount: [
    |                 "addr1"
    |               ],
    |               afterHeight: 0,
    |               afterTime: 0,
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
    |                 gt: 0,
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
    |               afterHeight: 0,
    |               afterTime: 0,
    |
    |               relatedAccount: [
    |                 "addr1"
    |               ]
    |             },
    |           },
    |
    |           {
    |             type: 4, # Block Rollback
    |             rules: {
    |               afterHeight: 0,
    |               afterTime: 0,
    |
    |               relatedAccount: [],
    |
    |               withTxsOfTypes: [
    |                 1,
    |               2
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

  settings.events map {aEvent =>
    
    aEvent match {
    case e: BlockAppendedEventSettings =>
      e.typeId should be(1) 
      e.typeDescription should be("Block Appended")
      e.withTxs should be(false)
      e.withMintingTxs should be(true)
      e.afterHeight should be(0)
      e.afterTime should be(0)
      
    case e: TxConfirmedEventSettings =>
      e.typeId should be(2)
      e.typeDescription should be("Tx Confirmed")
      e.afterHeight should be(0)
      e.afterTime should be(0)
      e.relatedAcc.get should be(Seq("addr1"))
      e.includeTypes.get should be(Seq(1, 2))
      e.excludeTypes.get should be(Seq(3, 4))
      e.amtGTE should be(0)
      e.amtGT should be(0)
      e.amtLTE should be(600000000000000000L)

    case e: StateUpdatedEventSettings =>
      e.typeId should be(3)
      e.typeDescription should be("State Updated")
      e.relatedAcc.get should be(Seq("addr1"))
      e.afterHeight should be(0)
      e.afterTime should be(0)

    case e: BlockRollbackEventSettings => 
      e.typeId should be(4)
      e.typeDescription should be("Block Rollback")
      e.relatedAcc.get should be(Seq.empty)
      e.afterHeight should be(0)
      e.afterTime should be(0)
      e.withTxsOfTypes.get should be(Seq(1, 2))
      e.withTxsOfAccs.get should be(Seq("addr3"))
      e.withStateOfAccs.get should be(Seq("addr4"))


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
    aEvent match{
    case e: BlockAppendedEventSettings => e.typeId should be(1)
    case e: TxConfirmedEventSettings => e.typeId should be(2)
    case e: StateUpdatedEventSettings => e.typeId should be(3)
    case e: BlockRollbackEventSettings => e.typeId should be(4)
    case _ => fail("The type description matching in setEventSettings goes wrong")
    }
  }
  }

  "WebhookSettings" should "return default Webhook setting when no webHooks provided" in {
  val defaultConfig = loadConfig(ConfigFactory.parseString(
    """vsys {
    | Event {
    |  enable = false
    |  
    | }
    |}""".stripMargin))

  val setInst = WebhookSettings.fromConfig(defaultConfig)(0)

  setInst.url should be("0.0.0.0")
  setInst.events should be(null)
  setInst.secretKey should be("")
  setInst.encryptKey should be("")
  setInst.maxSize should be(1000)
  }
}