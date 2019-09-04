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
    settings.networkSettings should be a[NetworkSettings]
    settings.walletSettings should be a[WalletSettings]
    settings.blockchainSettings should be a[BlockchainSettings]
    settings.checkpointsSettings should be a[CheckpointsSettings]
    settings.feesSettings should be a[FeesSettings]
    settings.matcherSettings should be a[MatcherSettings]
    settings.minerSettings should be a[MinerSettings]
    settings.restAPISettings should be a[RestAPISettings]
    settings.synchronizationSettings should be a[SynchronizationSettings]
    settings.utxSettings should be a[UtxSettings]
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
