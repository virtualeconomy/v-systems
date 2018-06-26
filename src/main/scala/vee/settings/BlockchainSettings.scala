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
  initialMintTime: Long,
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
    1529885280000000000L, 60.seconds)

  val TESTNET = GenesisSettings(1529898440087220090L, 1529898440087220090L, Constants.UnitsInWave * Constants.TotalWaves,
    ByteStr.decodeBase58("36uHjhrpSDuoC1zUkBsWyYKLwenTxykk5Bq2JcA3eBmop2anseJQDFBkAXWRgT828x8xyHVSQ4MuVCAqbKZt35ge").toOption,
    List(
      GenesisTransactionSettings("3N1YJ6RaYDkmh1fiy8ww7qCXDnySqyxceDS", (Constants.UnitsInWave * Constants.TotalWaves * 0.30).toLong),
      GenesisTransactionSettings("3NCorpZy4JhrtXtKeqLTft7Li79vehDssvr", (Constants.UnitsInWave * Constants.TotalWaves * 0.20).toLong),
      GenesisTransactionSettings("3MvRSHqRtn4sWgwr3EnDrP6VjphnQrrEB6t", (Constants.UnitsInWave * Constants.TotalWaves * 0.15).toLong),
      GenesisTransactionSettings("3MxPwccKXAp9bT9edNLRZHBvJhuEgrdJ61K", (Constants.UnitsInWave * Constants.TotalWaves * 0.05).toLong),
      GenesisTransactionSettings("3MzgaPu93fkmCqgkPZHLHgGt3pUZapuU3jM", (Constants.UnitsInWave * Constants.TotalWaves * 0.06).toLong),
      GenesisTransactionSettings("3N4SMepbKXPRADdjfUwNYKdcZdMoVJGXQP5", (Constants.UnitsInWave * Constants.TotalWaves * 0.06).toLong),
      GenesisTransactionSettings("3MxYTgmMWiaKT82y4jfZaSPDqEDN1JbETvp", (Constants.UnitsInWave * Constants.TotalWaves * 0.06).toLong),
      GenesisTransactionSettings("3MpZ718ivTCaRbra6JpABGV9Hdk75QAvpbj", (Constants.UnitsInWave * Constants.TotalWaves * 0.06).toLong),
      GenesisTransactionSettings("3N3SZdKP5qWv7AsKXDC1Vk7unWg81oQ3ynK", (Constants.UnitsInWave * Constants.TotalWaves * 0.04).toLong),
      GenesisTransactionSettings("3N15meHNxRzmfRYJJqrWA7p5NN2yd4CF62v", (Constants.UnitsInWave * Constants.TotalWaves * 0.02).toLong)),
    1529898440000000000L, 60.seconds)
}
