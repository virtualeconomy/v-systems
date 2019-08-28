package vsys.settings


import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import vsys.utils.ScorexLogging
import scala.util.{Try, Success, Failure}

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
      log.warn("Should Specify Webhook Config Under Event !!!")
      Seq(WebhookSettings("0.0.0.0", Seq.empty))

    } else {
      val hooks  = config.as[Seq[Config]](s"$configPath.webHooks") map {aHook => 
        val url: String = Try(aHook.as[String]("url")) match {
          case Success(s) => s
          case Failure(e) => 
            log.warn("Should Specify URL In Webhook Setting Under Event  !!!")
            "0.0.0.0"
        }

        val secret = aHook.as[Option[String]]("secret").getOrElse("")
        val encryptKey = aHook.as[Option[String]]("encryptKey").getOrElse("")
        val maxSize = aHook.as[Option[Int]]("maxSize").getOrElse(1000)

        val eventList = aHook.as[Seq[Config]]("events").map(conf =>
            WebhookEventSettings(
              conf,
              conf.as[String]("type")
            )
          ).filter(_.isRight).map(_.right.get)

        (url, eventList, secret, encryptKey, maxSize)
      }

      hooks.map(h => WebhookSettings(h._1, h._2, h._3, h._4, h._5))
    }
  }
}