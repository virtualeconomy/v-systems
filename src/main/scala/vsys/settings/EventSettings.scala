package vsys.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class EventSettings(webhookSettings: Seq[WebhookSettings], maxSize: Int)

object EventSettings {
  val configPath = "vsys.Event"

  def fromConfig(config: Config): EventSettings = {
    if(config.as[Option[Boolean]](s"$configPath.enable").getOrElse(false)) {
      val webhookSettings = WebhookSettings.fromConfig(config)
      val maxSize = config.as[Option[Int]](s"$configPath.maxSize").getOrElse(1000)
      EventSettings(webhookSettings, maxSize)
    } else {
      // no sending any event notification
      EventSettings(Seq.empty, 0)
    }
  }
}