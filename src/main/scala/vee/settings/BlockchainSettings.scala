package vee.settings

import scala.concurrent.duration._

import com.wavesplatform.settings.Constants
import com.wavesplatform.state2.ByteStr


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
        GenesisTransactionSettings("AR2vo3jQjoyJLQysg99AYTR1SQ5mHqGhS1P",15428574000000000L,-1),
        GenesisTransactionSettings("AR8ejrETNWLaABp27fYEdh291MR1kDC92ue",25097147000000000L,-1),
        GenesisTransactionSettings("ARBdeGKBfd2aJd5BEBJz2npX55nPVa4Tn6V",36000006000000000L,-1),
        GenesisTransactionSettings("ARAA6uz8dthMQaNSr3K4UhQY279UrJHQ37x",36000006000000000L,-1),
        GenesisTransactionSettings("ARGsRvusZpKpt4XFdVHAiWNCKmwoCG7Fm3n",36000006000000000L,-1),
        GenesisTransactionSettings("AR8fEYgWobHthXWNgUvhNbHaV3npVwqssM3",36000006000000000L,-1),
        GenesisTransactionSettings("AR5uQhUb5pp2PFLC6CMHdXzrSstaw55EQFb",15428574000000000L,-1),
        GenesisTransactionSettings("ARErPEJhWFzsnVAMDczUkYZ1LCbYRkPmYLT",36000006000000000L,-1),
        GenesisTransactionSettings("AR8HGzodYPzsUHtx15hfP2dytwN8t88HoXz",36000006000000000L,-1),
        GenesisTransactionSettings("ARMVYz7Nw2gJUrfLoQYZns9QgYTGnUnup9q",36000006000000000L,-1),
        GenesisTransactionSettings("AR5i6EcHeTAXxyAFwRvaT3VFBZW34hohj1C",26331433000000000L,-1),
        GenesisTransactionSettings("ARMhBmbgkFBpXAeGR3JEC6ynjKC4d83K7rH",36000006000000000L,-1),
        GenesisTransactionSettings("AR8DGjK1xrSAq3JvbvemCydsrByd5NgJPZP",36000006000000000L,-1),
        GenesisTransactionSettings("ARDX6cr3hdqAVUGH57bdNzj13wyWnXcTLWa",36000006000000000L,-1),
        GenesisTransactionSettings("ARQXTpJAxSME8G7eUuRhJM8MFtuXZhU8TZv",36000006000000000L,-1),
        GenesisTransactionSettings("AREi5xZQkffJdXbLmgmESYJZAkrGJUu6qBV",0L,0),
        GenesisTransactionSettings("AR3d9ELYQkfQpHut4hURAP5FQZ88iRHUAto",0L,4),
        GenesisTransactionSettings("ARLnNUEHqJshHPmauToNytZuWvXH5ZQk8jX",0L,8),
        GenesisTransactionSettings("AR9GT7DZyLxvofVLsoYtcrDzMYX6Q2VvXVU",0L,12),
        GenesisTransactionSettings("AR4v6KX8eTLTa7hGrNMVJsPWfqxWKh6Mf7o",0L,16),
        GenesisTransactionSettings("AR3JVT7Z4BvAjdUf7Y593WB7pFnEuQQuFFj",0L,20),
        GenesisTransactionSettings("ARKSxgC7m8S5uLAtYusWYTykd6YyUHUA5Uj",0L,24),
        GenesisTransactionSettings("ARLa44HK7iuvfJ7yCc1Y6rUJVCS9KSuLfxa",0L,28),
        GenesisTransactionSettings("AR9fDBBdQzeGmQY2ZikX2yUZMEMyjKEC5EW",0L,32),
        GenesisTransactionSettings("ARAsvWqn71V93NzMm7C7k2zkRwnpMLDRTR5",0L,36),
        GenesisTransactionSettings("ARQcD8MVftg2HhbufU2RWB9cTtnVe4b6BqG",0L,40),
        GenesisTransactionSettings("ARRf8LUEayHeYpqzmMTj6QB9iH76gh75qkg",0L,44),
        GenesisTransactionSettings("ARQ4rDViLmPT7oEgEX6JRpA6qWQXhLypEYx",0L,48),
        GenesisTransactionSettings("ARCkTMPANUYYZudAHTnJUjUYfV3UMnSqYCC",0L,52),
        GenesisTransactionSettings("ARPnxBFbMzQQn4SncJ2WdH61ynqcPcninV4",0L,56)),
    1543286357000000000L, 60.seconds)

  val TESTNET = GenesisSettings(1535356447650226656L, 1535356447650226656L, Constants.UnitsInVee * Constants.TotalVee,
    ByteStr.decodeBase58("5n4ewwZh9F4MMpSvtdxLCu5MUKnhEyUth2w3zEfpuiX3vwS1STNCdi51fmowJuLT1CfFg1DuodSvxwBZDANvGNej").toOption,
    List(
      GenesisTransactionSettings("ATxpELPa3yhE5h4XELxtPrW9TfXPrmYE7ze", (Constants.UnitsInVee * Constants.TotalVee * 0.30).toLong, 0),
      GenesisTransactionSettings("ATtRykARbyJS1RwNsA6Rn1Um3S7FuVSovHK", (Constants.UnitsInVee * Constants.TotalVee * 0.20).toLong, 1),
      GenesisTransactionSettings("ATtchuwHVQmNTsRA8ba19juGK9m1gNsUS1V", (Constants.UnitsInVee * Constants.TotalVee * 0.15).toLong, 2),
      GenesisTransactionSettings("AU4AoB2WzeXiJvgDhCZmr6B7uDqAzGymG3L", (Constants.UnitsInVee * Constants.TotalVee * 0.05).toLong, 3),
      GenesisTransactionSettings("AUBHchRBY4mVNktgCgJdGNcYbwvmzPKgBgN", (Constants.UnitsInVee * Constants.TotalVee * 0.06).toLong, 4),
      GenesisTransactionSettings("AU6qstXoazCHDK5dmuCqEnnTWgTqRugHwzm", (Constants.UnitsInVee * Constants.TotalVee * 0.06).toLong, 5),
      GenesisTransactionSettings("AU9HYFXuPZPbFVw8vmp7mFmHb7qiaMmgEYE", (Constants.UnitsInVee * Constants.TotalVee * 0.06).toLong, 6),
      GenesisTransactionSettings("AUBLPMpHVV74fHQD8D6KosA76nusw4FqRr1", (Constants.UnitsInVee * Constants.TotalVee * 0.06).toLong, 7),
      GenesisTransactionSettings("AUBbpPbymsrM8QiXqS3NU7CrD1vy1EyonCa", (Constants.UnitsInVee * Constants.TotalVee * 0.04).toLong, 8),
      GenesisTransactionSettings("AU7nJLcT1mThXGTT1KDkoAtfPzc82Sgay1V", (Constants.UnitsInVee * Constants.TotalVee * 0.02).toLong, 9)),
    1535356440000000000L, 60.seconds)
}
