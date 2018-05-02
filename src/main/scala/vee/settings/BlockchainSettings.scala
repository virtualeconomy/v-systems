package vee.settings

import scala.concurrent.duration._

import com.wavesplatform.settings.Constants
import com.wavesplatform.state2.ByteStr


case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(
  blockTimestamp: Long,
  timestamp: Long,
  initialBalance: Long,
  signature: Option[ByteStr],
  transactions: Seq[GenesisTransactionSettings],
  initialBaseTarget: Long,
  averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(1460678400000L, 1465742577614L, Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2").toOption,
    List(
      GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", Constants.UnitsInWave * Constants.TotalWaves - 5 * Constants.UnitsInWave),
      GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", Constants.UnitsInWave),
      GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", Constants.UnitsInWave),
      GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", Constants.UnitsInWave),
      GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", Constants.UnitsInWave),
      GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", Constants.UnitsInWave)),
    153722867L, 60.seconds)

  val TESTNET = GenesisSettings(1521577973210L, 1521577973210L, Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa").toOption,
    List(
      GenesisTransactionSettings("3N1YJ6RaYDkmh1fiy8ww7qCXDnySqyxceDS", (Constants.UnitsInWave * Constants.TotalWaves * 0.65).toLong),
      GenesisTransactionSettings("3NCorpZy4JhrtXtKeqLTft7Li79vehDssvr", (Constants.UnitsInWave * Constants.TotalWaves * 0.20).toLong),
      GenesisTransactionSettings("3MvRSHqRtn4sWgwr3EnDrP6VjphnQrrEB6t", (Constants.UnitsInWave * Constants.TotalWaves * 0.15).toLong)),
    153722867L, 60.seconds)
}

