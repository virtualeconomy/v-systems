package vsys.settings


import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import vsys.utils.ScorexLogging
import scala.util.{Try, Success, Failure}

case class WebhookSettings(url: String,
            events: Seq[WebhookEventSettings],
            secretKey: Option[String] = None,
            encryptKey: Option[String] = None,
            maxSize: Option[Int] = None)

object WebhookSettings extends ScorexLogging {
  val configPath = "vsys.Event"

  def fromConfig(config: Config): Seq[WebhookSettings] = {

    if(config.hasPath(s"$configPath.webHooks")) {

      val hooks = config.as[Seq[Config]](s"$configPath.webHooks") map {aHook =>
        val url: String = Try(aHook.as[String]("url")) match {
          case Success(s) => s
          case Failure(e) =>
            log.error("Should Specify URL In Webhook Setting Under Event !!!")
            ""
        }

        val secret = Try(aHook.as[String]("secret")).toOption
        val encryptKey = Try(aHook.as[String]("encryptKey")).toOption
        val maxSize = Try(aHook.as[Int]("maxSize")).toOption

        val eventList = aHook.as[Option[Seq[Config]]]("events").getOrElse(Seq.empty).map(conf =>
            WebhookEventSettings(conf, conf.as[String]("type"))
          ).filter(_.isRight).map(_.right.get)

        (url, eventList, secret, encryptKey, maxSize)
      }

      hooks.map(h => (WebhookSettings.apply _).tupled(h))

    } else {
      // send all event notifications
      log.warn("Should Specify Webhook Config Under Event !!!")
      Seq(WebhookSettings("", Seq.empty))
    }
  }
}
