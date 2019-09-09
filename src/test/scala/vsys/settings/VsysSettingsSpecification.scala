package vsys.settings

import java.io.File

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class VsysSettingsSpecification extends FlatSpec with Matchers {
  private val home = System.getProperty("user.home")

  "VsysSettings" should "read values from default config" in {
    val config = ConfigFactory.load()
    val settings = VsysSettings.fromConfig(config)

    settings.directory should be(home + "/.vsys")
    settings.loggingLevel should be(LogLevel.INFO)
    settings.networkSettings shouldBe a[NetworkSettings]
    settings.walletSettings shouldBe a[WalletSettings]
    settings.blockchainSettings shouldBe a[BlockchainSettings]
    settings.checkpointsSettings shouldBe a[CheckpointsSettings]
    settings.feesSettings shouldBe a[FeesSettings]
    settings.matcherSettings shouldBe a[MatcherSettings]
    settings.minerSettings shouldBe a[MinerSettings]
    settings.restAPISettings shouldBe a[RestAPISettings]
    settings.synchronizationSettings shouldBe a[SynchronizationSettings]
    settings.utxSettings shouldBe a[UtxSettings]
  }

  "VsysSettings" should "resolver folders correctly" in {
    val config = loadConfig(ConfigFactory.parseString(
      """vsys {
        |  logging-level = TRACE
        |  directory = "/xxx"
        |}""".stripMargin))

    val settings = VsysSettings.fromConfig(config.resolve())

    settings.directory should be("/xxx")
    settings.networkSettings.file should be(Some(new File("/xxx/data/peers.dat")))
    settings.walletSettings.file should be(Some(new File("/xxx/wallet/wallet.dat")))
    settings.loggingLevel should be(LogLevel.TRACE)
    settings.matcherSettings.journalDataDir should be ("/xxx/matcher/journal")
    settings.matcherSettings.snapshotsDataDir should be ("/xxx/matcher/snapshots")
  }

}
