package vsys.settings

import com.typesafe.config.ConfigException.BadValue
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class FeesSettingsSpecification extends FlatSpec with Matchers {
  "FeesSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """vsys {
        |  network.file = "xxx"
        |  fees {
        |    payment.VSYS = 100000
        |    issue.VSYS = 100000000
        |    transfer.VSYS = 100000
        |    reissue.VSYS = 100000
        |    burn.VSYS = 100000
        |    exchange.VSYS = 100000
        |  }
        |  miner.timeout = 10
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(6)

    settings.fees(2) should be(List(FeeSettings("VSYS", 100000)))
    settings.fees(11) should be(List(FeeSettings("VSYS", 100000000)))
    settings.fees(12) should be(List(FeeSettings("VSYS", 100000)))
    settings.fees(13) should be(List(FeeSettings("VSYS", 100000)))
    settings.fees(14) should be(List(FeeSettings("VSYS", 100000)))
    settings.fees(15) should be(List(FeeSettings("VSYS", 100000)))
  }

  it should "combine read few fees for one transaction type" in {
    val config = ConfigFactory.parseString(
      """vsys.blockchain.blocks {
        |  payment {
        |    VSYS0 = 0
        |  }
        |  issue {
        |    VSYS1 = 111
        |    VSYS2 = 222
        |    VSYS3 = 333
        |  }
        |  transfer {
        |    VSYS4 = 444
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(3)
    settings.fees(2).toSet should equal(Set(FeeSettings("VSYS0", 0)))
    settings.fees(11).toSet should equal(Set(FeeSettings("VSYS1", 111), FeeSettings("VSYS2", 222), FeeSettings("VSYS3", 333)))
    settings.fees(12).toSet should equal(Set(FeeSettings("VSYS4", 444)))
  }

  it should "allow empty list" in {
    val config = ConfigFactory.parseString("vsys.blockchain.blocks = {}").resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(0)
  }

  it should "override values" in {
    val config = ConfigFactory.parseString(
      """vsys.blockchain.blocks {
        |  payment.VSYS1 = 1111
        |  reissue.VSYS5 = 0
        |}
      """.stripMargin).withFallback(
      ConfigFactory.parseString(
        """vsys.blockchain.blocks {
          |  payment.VSYS = 100000
          |  issue.VSYS = 100000000
          |  transfer.VSYS = 100000
          |  reissue.VSYS = 100000
          |  burn.VSYS = 100000
          |  exchange.VSYS = 100000
          |}
        """.stripMargin)
    ).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(6)
    settings.fees(2).toSet should equal(Set(FeeSettings("VSYS", 100000), FeeSettings("VSYS1", 1111)))
    settings.fees(13).toSet should equal(Set(FeeSettings("VSYS", 100000), FeeSettings("VSYS5", 0)))
  }

  it should "fail on incorrect long values" in {
    val config = ConfigFactory.parseString("vsys.blockchain.blocks.payment.VSYS=N/A").resolve()

    intercept[BadValue] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "fail on unknown transaction type" in {
    val config = ConfigFactory.parseString("vsys.blockchain.blocks.shmayment.VSYS=100").resolve()

    intercept[NoSuchElementException] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "override values from default config" in {
    val defaultConfig = ConfigFactory.load()
    val config = ConfigFactory.parseString(
      """
        |vsys.blockchain.blocks {
        |  payment {
        |    VSYS = 10000000
        |  }
        |  issue {
        |    VSYS = 100000000
        |  }
        |  transfer {
        |    VSYS = 10000000,
        |    "6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL" = 1
        |  }
        |  reissue {
        |    VSYS = 10000000
        |  }
        |  burn {
        |    VSYS = 10000000
        |  }
        |  exchange {
        |    VSYS = 10000000
        |  }
        |  lease {
        |    VSYS = 10000000
        |  }
        |  lease-cancel {
        |    VSYS = 10000000
        |  }
        |  create-alias {
        |    VSYS = 10000000
        |  }
        |  contend-slots {
        |    VSYS = 5000000000000
        |  }
        |  release-slots {
        |    VSYS = 10000000
        |  }
        |  register-contract{
        |    VSYS = 10000000000
        |  }
        |  execute-contract-function{
        |    VSYS = 30000000
        |  }
        |  db-put{
        |    VSYS = 100000000
        |  }
        |}
      """.stripMargin).withFallback(defaultConfig).resolve()
    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(14)

    settings.fees(2).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(3).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(4).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(6).toSet should equal(Set(FeeSettings("VSYS", 5000000000000L)))
    settings.fees(7).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(8).toSet should equal(Set(FeeSettings("VSYS", 10000000000L)))
    settings.fees(9).toSet should equal(Set(FeeSettings("VSYS", 30000000)))
    settings.fees(10).toSet should equal(Set(FeeSettings("VSYS", 100000000)))
    settings.fees(11).toSet should equal(Set(FeeSettings("VSYS", 100000000)))
    settings.fees(12).toSet should equal(Set(FeeSettings("VSYS", 10000000), FeeSettings("6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL", 1)))
    settings.fees(13).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(14).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(15).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(16).toSet should equal(Set(FeeSettings("VSYS", 10000000)))

  }
}
