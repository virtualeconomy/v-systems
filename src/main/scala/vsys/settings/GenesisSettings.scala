package vsys.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.settings.{Constants, loadConfig}
import com.wavesplatform.state2.ByteStr

import scala.concurrent.duration._


case class GenesisTransactionSettings(recipient: String, amount: Long, slotId: Int)

case class GenesisSettings(
  blockTimestamp: Long,
  timestamp: Long,
  initialBalance: Long,
  signature: Option[ByteStr],
  transactions: Seq[GenesisTransactionSettings],
  initialMintTime: Long,
  averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(1543286357457333127L, 1543286357457333127L, 514285800000000000L,
    ByteStr.decodeBase58("3yYNd7quEWaWytrAug4yGwQvpL3PVJegf9d9NTv9PVE3ouBYJs5PTQqxCjd294uK1zPLj6G5Tk447LqFMWdSFvaQ").toOption,
    List(
      GenesisTransactionSettings("ARKwwhnX2mk9V79kuvb3tEWVyri5Z2HFsPR", 36000006000000000L, -1),
      GenesisTransactionSettings("AR2vo3jQjoyJLQysg99AYTR1SQ5mHqGhS1P", 15428574000000000L, -1),
      GenesisTransactionSettings("AR8ejrETNWLaABp27fYEdh291MR1kDC92ue", 25097147000000000L, -1),
      GenesisTransactionSettings("ARBdeGKBfd2aJd5BEBJz2npX55nPVa4Tn6V", 36000006000000000L, -1),
      GenesisTransactionSettings("ARAA6uz8dthMQaNSr3K4UhQY279UrJHQ37x", 36000006000000000L, -1),
      GenesisTransactionSettings("ARGsRvusZpKpt4XFdVHAiWNCKmwoCG7Fm3n", 36000006000000000L, -1),
      GenesisTransactionSettings("AR8fEYgWobHthXWNgUvhNbHaV3npVwqssM3", 36000006000000000L, -1),
      GenesisTransactionSettings("AR5uQhUb5pp2PFLC6CMHdXzrSstaw55EQFb", 15428574000000000L, -1),
      GenesisTransactionSettings("ARErPEJhWFzsnVAMDczUkYZ1LCbYRkPmYLT", 36000006000000000L, -1),
      GenesisTransactionSettings("AR8HGzodYPzsUHtx15hfP2dytwN8t88HoXz", 36000006000000000L, -1),
      GenesisTransactionSettings("ARMVYz7Nw2gJUrfLoQYZns9QgYTGnUnup9q", 36000006000000000L, -1),
      GenesisTransactionSettings("AR5i6EcHeTAXxyAFwRvaT3VFBZW34hohj1C", 26331433000000000L, -1),
      GenesisTransactionSettings("ARMhBmbgkFBpXAeGR3JEC6ynjKC4d83K7rH", 36000006000000000L, -1),
      GenesisTransactionSettings("AR8DGjK1xrSAq3JvbvemCydsrByd5NgJPZP", 36000006000000000L, -1),
      GenesisTransactionSettings("ARDX6cr3hdqAVUGH57bdNzj13wyWnXcTLWa", 36000006000000000L, -1),
      GenesisTransactionSettings("ARQXTpJAxSME8G7eUuRhJM8MFtuXZhU8TZv", 36000006000000000L, -1),
      GenesisTransactionSettings("AREi5xZQkffJdXbLmgmESYJZAkrGJUu6qBV", 0L, 0),
      GenesisTransactionSettings("AR3d9ELYQkfQpHut4hURAP5FQZ88iRHUAto", 0L, 4),
      GenesisTransactionSettings("ARLnNUEHqJshHPmauToNytZuWvXH5ZQk8jX", 0L, 8),
      GenesisTransactionSettings("AR9GT7DZyLxvofVLsoYtcrDzMYX6Q2VvXVU", 0L, 12),
      GenesisTransactionSettings("AR4v6KX8eTLTa7hGrNMVJsPWfqxWKh6Mf7o", 0L, 16),
      GenesisTransactionSettings("AR3JVT7Z4BvAjdUf7Y593WB7pFnEuQQuFFj", 0L, 20),
      GenesisTransactionSettings("ARKSxgC7m8S5uLAtYusWYTykd6YyUHUA5Uj", 0L, 24),
      GenesisTransactionSettings("ARLa44HK7iuvfJ7yCc1Y6rUJVCS9KSuLfxa", 0L, 28),
      GenesisTransactionSettings("AR9fDBBdQzeGmQY2ZikX2yUZMEMyjKEC5EW", 0L, 32),
      GenesisTransactionSettings("ARAsvWqn71V93NzMm7C7k2zkRwnpMLDRTR5", 0L, 36),
      GenesisTransactionSettings("ARQcD8MVftg2HhbufU2RWB9cTtnVe4b6BqG", 0L, 40),
      GenesisTransactionSettings("ARRf8LUEayHeYpqzmMTj6QB9iH76gh75qkg", 0L, 44),
      GenesisTransactionSettings("ARQ4rDViLmPT7oEgEX6JRpA6qWQXhLypEYx", 0L, 48),
      GenesisTransactionSettings("ARCkTMPANUYYZudAHTnJUjUYfV3UMnSqYCC", 0L, 52),
      GenesisTransactionSettings("ARPnxBFbMzQQn4SncJ2WdH61ynqcPcninV4", 0L, 56)),
    1543286357000000000L, 60.seconds)


  val file = new File("testnet_easy_start.conf")
  var cfg: Config = ConfigFactory.parseFile(file)
  loadConfig(cfg)

  val miner_num: Int = cfg.getInt("vsys.miner.quorum")
  val peer_num: Int = cfg.getInt("vsys.peer.quorum")
  val addresses: Config = cfg.getConfig("vsys.wallet.slots")

  var genesis_transaction_list: List[GenesisTransactionSettings] = Nil

  for (miner_index <- 0 until miner_num + peer_num) {
    if (genesis_transaction_list.isEmpty){
      genesis_transaction_list = List(GenesisTransactionSettings(addresses.getConfig("slot" + miner_index.toString).getString("address"), (Constants.UnitsInVsys * Constants.TotalVsys * addresses.getConfig("slot" + miner_index.toString).getDouble("balance_distribution")).toLong, miner_index))
    }
    else
      genesis_transaction_list ::= GenesisTransactionSettings(addresses.getConfig("slot" + miner_index.toString).getString("address"), (Constants.UnitsInVsys * Constants.TotalVsys * addresses.getConfig("slot" + miner_index.toString).getDouble("balance_distribution")).toLong, miner_index)

  }

  val TESTNET = GenesisSettings(1535356447650226656L, 1535356447650226656L, Constants.UnitsInVsys * Constants.TotalVsys,
    ByteStr.decodeBase58("5n4ewwZh9F4MMpSvtdxLCu5MUKnhEyUth2w3zEfpuiX3vwS1STNCdi51fmowJuLT1CfFg1DuodSvxwBZDANvGNej").toOption, genesis_transaction_list, 1535356440000000000L, 60.seconds)

}
