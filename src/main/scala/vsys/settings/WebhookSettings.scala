package vsys.settings


import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import vsys.utils.ScorexLogging

case class WebhookSettings(url: String,
            events: Seq[WebhookEventSettings],
            secretKey: String = "",
            encryptKey: String = "",
            maxSize: Int = 1000)

object WebhookSettings extends ScorexLogging {
  val configPath = "vsys.Event"

  def fromConfig(config: Config): Seq[WebhookSettings] = {
    
    if(!config.hasPath(s"$configPath.webHooks")) {
      //send all event notifications
      Seq(WebhookSettings("0.0.0.0", null))

    } else {
      //config.getObject(s"$configPath.webHooks").asScala.foreach({case (k, v) => log.info(k.toString + v.toString)})
      val hooks  = config.as[Seq[Config]](s"$configPath.webHooks") map {aHook => 
        val url = aHook.as[String]("url")
        val secret = aHook.as[Option[String]]("secret").getOrElse("")
        val encryptKey = aHook.as[Option[String]]("encryptKey").getOrElse("")
        val maxSize = aHook.as[Option[Int]]("maxSize").getOrElse(1000)

        val eventList = aHook.as[Seq[Config]]("events").map(eventConfig => 
            WebhookEventSettings(
              eventConfig,
              eventConfig.as[String]("type")
            )
          ).filter(_.isRight).map(_.right.get)

        
        (url, eventList, secret, encryptKey, maxSize)

      }
      hooks.map(h => WebhookSettings(h._1, Seq.empty, h._3, h._4, h._5))
    }
  }
}