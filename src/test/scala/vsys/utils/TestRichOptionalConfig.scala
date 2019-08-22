package vsys.utils

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}
import vsys.settings.loadConfig


class TestRichOptionalConfig extends FlatSpec with Matchers {
  "RichConfigVal" should "get value from config" in {
    import vsys.utils.RichOptionalConfig.RichConfigVal

    val path = "vsys.Event"
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        | Event {
        |   testBoolean: false,
        |   testInt: 1,
        |   testLong: 2,
        |   testString: "yes"
        | }
        |}""".stripMargin))

    config.getConfBoolean(s"$path.testBoolean", true) should be(false)
    config.getConfInt(s"$path.testInt", 0) should be(1)
    config.getConfLong(s"$path.testLong", 600000000000000000L) should be(2)
    config.getConfString(s"$path.testString", "no") should be("yes")

  }

  it should "get default value" in {
    import vsys.utils.RichOptionalConfig.RichConfigVal

    val path = "vsys.Event"
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        | Event {}
        |}""".stripMargin))

    config.getConfBoolean(s"$path.testBoolean", true) should be(true)
    config.getConfInt(s"$path.testInt", 0) should be(0)
    config.getConfLong(s"$path.testLong", 600000000000000000L) should be(600000000000000000L)
    config.getConfString(s"$path.testString", "no") should be("no")
  }

  "RichConfigRef" should "get value from config" in {
    import vsys.utils.RichOptionalConfig.RichConfigRef

    val path = "vsys.Event"
    val config = loadConfig(ConfigFactory.parseString(
      """vsys{
        | Event {
        |   testSeqInt: [
        |     1,
        |     2,
        |     3
        |   ],
        | 
        |   testSeqString: []
        |
        | }
        |}""".stripMargin))

    config.getConfSeqInt(s"$path.testSeqInt", null) should be(Option(Seq(1, 2, 3)))
    config.getConfSeqString(s"$path.testSeqString", Seq("b")) should be(Option(Seq()))
  }

  it should "get default value" in {
    import vsys.utils.RichOptionalConfig.RichConfigRef

    val path = "vsys.Event"
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        | Event {}
        |}""".stripMargin))

    config.getConfSeqInt(s"$path.testSeqInt", null) should be(None)
    config.getConfSeqString(s"path.testSeqString", null) should be(None)
  }
}