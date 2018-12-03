package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.scalatest.{FlatSpec, Matchers}

class WalletSettingsSpecification extends FlatSpec with Matchers {
  "WalletSettings" should "read values from config" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys.wallet {
        |  file: /vsys/wallet/wallet.dat
        |  password: "some string as password"
        |  seed: "BASE58SEED"
        |}""".stripMargin))
    val settings = config.as[WalletSettings]("vsys.wallet")

    settings.seed should be(Some("BASE58SEED"))
    settings.file should be(Some(new File("/vsys/wallet/wallet.dat")))
    settings.password should be("some string as password")
  }
}
