package vsys.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._

import vsys.blockchain.state.ByteStr

case class CheckpointsSettings(publicKey: ByteStr)

object CheckpointsSettings {
  val configPath: String = "vsys.checkpoints"

  def fromConfig(config: Config): CheckpointsSettings = {
    val publicKey = config.as[ByteStr](s"$configPath.public-key")

    CheckpointsSettings(publicKey)
  }
}