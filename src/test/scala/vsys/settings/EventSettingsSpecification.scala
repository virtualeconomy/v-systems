package vsys.settings

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class EventSettingsSpecification extends FlatSpec with Matchers {

  "EventSettings" should "return default value" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        | Event {}
        |}""".stripMargin))

    val settings = EventSettings.fromConfig(config)
    settings.webhookSettings should be(Seq.empty)
  }

  it should "get custom websettings which block notification" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        | Event {
        |   enable: true,
        |   webHooks: []
        | }
        |}""".stripMargin))

    val settings = EventSettings.fromConfig(config)
    settings.webhookSettings should be(Seq.empty)
  }
}