package com.wavesplatform.settings

import com.typesafe.config.ConfigException.BadValue
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class FeesSettingsSpecification extends FlatSpec with Matchers {
  "FeesSettings" should "read values" in {
    val config = ConfigFactory.parseString(
      """vee {
        |  network.file = "xxx"
        |  fees {
        |    payment.VEE = 100000
        |    issue.VEE = 100000000
        |    transfer.VEE = 100000
        |    reissue.VEE = 100000
        |    burn.VEE = 100000
        |    exchange.VEE = 100000
        |  }
        |  miner.timeout = 10
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(6)

    settings.fees(2) should be(List(FeeSettings("VEE", 100000)))
    settings.fees(11) should be(List(FeeSettings("VEE", 100000000)))
    settings.fees(12) should be(List(FeeSettings("VEE", 100000)))
    settings.fees(13) should be(List(FeeSettings("VEE", 100000)))
    settings.fees(14) should be(List(FeeSettings("VEE", 100000)))
    settings.fees(15) should be(List(FeeSettings("VEE", 100000)))
  }

  it should "combine read few fees for one transaction type" in {
    val config = ConfigFactory.parseString(
      """vee.fees {
        |  payment {
        |    VEE0 = 0
        |  }
        |  issue {
        |    VEE1 = 111
        |    VEE2 = 222
        |    VEE3 = 333
        |  }
        |  transfer {
        |    VEE4 = 444
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(3)
    settings.fees(2).toSet should equal(Set(FeeSettings("VEE0", 0)))
    settings.fees(11).toSet should equal(Set(FeeSettings("VEE1", 111), FeeSettings("VEE2", 222), FeeSettings("VEE3", 333)))
    settings.fees(12).toSet should equal(Set(FeeSettings("VEE4", 444)))
  }

  it should "allow empty list" in {
    val config = ConfigFactory.parseString("vee.fees = {}").resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(0)
  }

  it should "override values" in {
    val config = ConfigFactory.parseString(
      """vee.fees {
        |  payment.VEE1 = 1111
        |  reissue.VEE5 = 0
        |}
      """.stripMargin).withFallback(
      ConfigFactory.parseString(
        """vee.fees {
          |  payment.VEE = 100000
          |  issue.VEE = 100000000
          |  transfer.VEE = 100000
          |  reissue.VEE = 100000
          |  burn.VEE = 100000
          |  exchange.VEE = 100000
          |}
        """.stripMargin)
    ).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(6)
    settings.fees(2).toSet should equal(Set(FeeSettings("VEE", 100000), FeeSettings("VEE1", 1111)))
    settings.fees(13).toSet should equal(Set(FeeSettings("VEE", 100000), FeeSettings("VEE5", 0)))
  }

  it should "fail on incorrect long values" in {
    val config = ConfigFactory.parseString("vee.fees.payment.VEE=N/A").resolve()

    intercept[BadValue] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "fail on unknown transaction type" in {
    val config = ConfigFactory.parseString("vee.fees.shmayment.VEE=100").resolve()

    intercept[NoSuchElementException] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "override values from default config" in {
    val defaultConfig = ConfigFactory.load()
    val config = ConfigFactory.parseString(
      """
        |vee.fees {
        |  payment {
        |    VEE = 10000000
        |  }
        |  issue {
        |    VEE = 100000000
        |  }
        |  transfer {
        |    VEE = 10000000,
        |    "6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL" = 1
        |  }
        |  reissue {
        |    VEE = 10000000
        |  }
        |  burn {
        |    VEE = 10000000
        |  }
        |  exchange {
        |    VEE = 10000000
        |  }
        |  lease {
        |    VEE = 10000000
        |  }
        |  lease-cancel {
        |    VEE = 10000000
        |  }
        |  create-alias {
        |    VEE = 10000000
        |  }
        |  contend-slots {
        |    VEE = 100000000000
        |  }
        |  release-slots {
        |    VEE = 10000000
        |  }
        |  minting {
        |    VEE = 100000
        |  }
        |  create-contract{
        |    VEE = 20000000
        |  }
        |  change-contract-status{
        |    VEE = 10000000
        |  }
        |  db-put{
        |    VEE = 100000000
        |  }
        |}
      """.stripMargin).withFallback(defaultConfig).resolve()
    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(15)

    settings.fees(2).toSet should equal(Set(FeeSettings("VEE", 10000000)))
    settings.fees(3).toSet should equal(Set(FeeSettings("VEE", 10000000)))
    settings.fees(4).toSet should equal(Set(FeeSettings("VEE", 10000000)))
    settings.fees(5).toSet should equal(Set(FeeSettings("VEE", 100000)))
    settings.fees(6).toSet should equal(Set(FeeSettings("VEE", 100000000000L)))
    settings.fees(7).toSet should equal(Set(FeeSettings("VEE", 10000000)))
    settings.fees(8).toSet should equal(Set(FeeSettings("VEE", 20000000)))
    settings.fees(9).toSet should equal(Set(FeeSettings("VEE", 10000000)))
    settings.fees(10).toSet should equal(Set(FeeSettings("VEE", 100000000)))
    settings.fees(11).toSet should equal(Set(FeeSettings("VEE", 100000000)))
    settings.fees(12).toSet should equal(Set(FeeSettings("VEE", 10000000), FeeSettings("6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL", 1)))
    settings.fees(13).toSet should equal(Set(FeeSettings("VEE", 10000000)))
    settings.fees(14).toSet should equal(Set(FeeSettings("VEE", 10000000)))
    settings.fees(15).toSet should equal(Set(FeeSettings("VEE", 10000000)))
    settings.fees(16).toSet should equal(Set(FeeSettings("VEE", 10000000)))

  }
}
