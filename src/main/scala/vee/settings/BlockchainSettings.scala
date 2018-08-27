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
  val MAINNET = GenesisSettings(1460678400000L, 1465742577614L, Constants.UnitsInVee * Constants.TotalVee,
    ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption,
    List(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", Constants.UnitsInVee * Constants.TotalVee - 5 * Constants.UnitsInVee, 0),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", Constants.UnitsInVee, 1),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", Constants.UnitsInVee, 2),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", Constants.UnitsInVee, 3),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", Constants.UnitsInVee, 4 ),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", Constants.UnitsInVee, 5)),
    1529885280000000000L, 60.seconds)

  val TESTNET = GenesisSettings(1535078012713428395L, 1535078012713428395L, Constants.UnitsInVee * Constants.TotalVee,
    ByteStr.decodeBase58("41ay4EKgGwqhQcJ4beAX8JDpWBsKV3iMp3bka3k412oP1JAGUiizWVFtjmk7wwN9bYJXCS9h23jrLK9w4EmENoSG").toOption,
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
    1535078010000000000L, 60.seconds)
}
