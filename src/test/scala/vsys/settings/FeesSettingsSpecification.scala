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
        |  }
        |  miner.timeout = 10
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(1)

    settings.fees(2) should be(List(FeeSettings("VSYS", 100000)))
  }

  it should "allow empty list" in {
    val config = ConfigFactory.parseString("vsys.fees = {}").resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(0)
  }

  it should "override values" in {
    val config = ConfigFactory.parseString(
      """vsys.fees {
        |  payment.VSYS = 1111
        |}
      """.stripMargin).withFallback(
      ConfigFactory.parseString(
        """vsys.fees {
          |  payment.VSYS = 100000
          |  lease.VSYS = 10000
          |}
        """.stripMargin)
    ).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(2)
    settings.fees(2).toSet should equal(Set(FeeSettings("VSYS", 1111)))
    settings.fees(3).toSet should equal(Set(FeeSettings("VSYS", 10000)))
  }

  it should "fail on incorrect long values" in {
    val config = ConfigFactory.parseString("vsys.fees.payment.VSYS=N/A").resolve()

    intercept[BadValue] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "fail on unknown transaction type" in {
    val config = ConfigFactory.parseString("vsys.fees.shmayment.VSYS=100").resolve()

    intercept[NoSuchElementException] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "override values from default config" in {
    val defaultConfig = ConfigFactory.load()
    val config = ConfigFactory.parseString(
      """
        |vsys.fees {
        |  payment {
        |    VSYS = 10000000
        |  }
        |  lease {
        |    VSYS = 10000000
        |  }
        |  lease-cancel {
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
    settings.fees.size should be(8)

    settings.fees(2).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(3).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(4).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(6).toSet should equal(Set(FeeSettings("VSYS", 5000000000000L)))
    settings.fees(7).toSet should equal(Set(FeeSettings("VSYS", 10000000)))
    settings.fees(8).toSet should equal(Set(FeeSettings("VSYS", 10000000000L)))
    settings.fees(9).toSet should equal(Set(FeeSettings("VSYS", 30000000)))
    settings.fees(10).toSet should equal(Set(FeeSettings("VSYS", 100000000)))

  }
}
