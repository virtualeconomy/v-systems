package vsys.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class EventSettings(webhookSettings: Seq[WebhookSettings])

object EventSettings {
  val configPath = "vsys.Event"

  def fromConfig(config: Config): EventSettings = {
    if(config.as[Option[Boolean]](s"$configPath.enable").getOrElse(false)) {
      val webhookSettings = WebhookSettings.fromConfig(config)
      EventSettings(webhookSettings)
    } else {
      // no sending any event notification
      EventSettings(Seq.empty)
    }
  }
}