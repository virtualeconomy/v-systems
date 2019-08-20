package vsys.settings


import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import vsys.blockchain.transaction.ValidationError
import vsys.utils.ScorexLogging

//import scala.collection.JavaConversions._

case class WebhookSettings(url: String,
						events: Seq[WebhookEventSettings],
						secretKey: String = "",
						encryptKey: String = "",
						maxSize: Int = 1000
						)

object WebhookSettings extends ScorexLogging{
	val configPath = "vsys.Event"

	def fromConfig(config: Config): Seq[WebhookSettings] = {
		val hooks  = config.as[Seq[Config]](s"$configPath.webHooks") map {aHook => 
			val url = aHook.as[String]("url")
			val secret = aHook.as[String]("secret")
			val encryptKey = aHook.as[String]("encryptKey")
			val maxSize = aHook.as[Int]("maxSize")
			
			val eventList = aHook.as[Seq[Config]]("events").map(eventConfig =>
				setEventSettings(eventConfig)
			).filter(_.isRight).map(_.right.get)

			(url, eventList, secret, encryptKey, maxSize)

		}
		hooks.map(h => WebhookSettings(h._1, h._2, h._3, h._4, h._5))
	}

	private def setEventSettings(config: Config): Either[ValidationError, WebhookEventSettings] = {
		val path = "rules"

		config.as[String]("type") match {
			case "1" | "Block Appended" => 
				Right(BlockAppendedEventSettings(config.as[Boolean](s"$path.withTxs"), 
										   		 config.as[Long](s"$path.afterHeight"),
										   		 config.as[Long](s"$path.afterTime")))
			case "2" | "Tx Confirmed" =>
				Right(TxConfirmedEventSettings(config.as[Seq[String]](s"$path.relatedAccount"),
										 	   config.as[Long](s"$path.afterHeight"),
											   config.as[Long](s"$path.afterTime"),
											   config.as[Seq[Int]](s"$path.includeTypes"),
											   config.as[Seq[Int]](s"$path.excludeTypes"),
											   config.as[Long](s"$path.amount.gte"),
											   config.as[Long](s"$path.amount.gt"),
											   config.as[Long](s"$path.amount.lte"),
											   config.as[Long](s"$path.amount.lt"),
											   config.as[Boolean](s"$path.amount.withFee")) )

			case "3" | "State Updated" =>
				Right(StateUpdatedEventSettings(config.as[Seq[String]](s"$path.relatedAccount"),
											 	config.as[Long](s"$path.afterHeight"),
											 	config.as[Long](s"$path.afterTime")))

			case "4" | "Block Rollback" => 
				Right(BlockRollbackEventSettings(config.as[Seq[String]](s"$path.relatedAccount"),
											   	 config.as[Long](s"$path.afterHeight"),
											   	 config.as[Long](s"$path.afterTime"),
											   	 config.as[Seq[Int]](s"$path.withTxsOfTypes"),
											   	 config.as[Seq[String]](s"$path.withTxsOfAccs"),
											   	 config.as[Seq[String]](s"$path.withStateOfAccs")))
			case _ => 
				Left(ValidationError.InvalidEventTypeError)
			
		}
	}
}