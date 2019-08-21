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
            maxSize: Int = 1000)

object WebhookSettings extends ScorexLogging{
  import vsys.utils.RichOptionalConfig._
  val configPath = "vsys.Event"

  def fromConfig(config: Config): Seq[WebhookSettings] = {
    val isEnable = config.getConfBoolean(s"$configPath.enable", false)
    
    if(!config.hasPath(s"$configPath.webHooks") && !isEnable){
      Seq(WebhookSettings("0.0.0.0", null))
    }else{
      val hooks  = config.as[Seq[Config]](s"$configPath.webHooks") map {aHook => 
        val url = aHook.as[String]("url")
        val secret = aHook.getConfString("secret", "")
        val encryptKey = aHook.getConfString("encryptKey", "")
        val maxSize = aHook.getConfInt("maxSize", 1000)
        
        val eventList = aHook.as[Seq[Config]]("events").map(eventConfig =>
          setEventSettings(eventConfig)
        ).filter(_.isRight).map(_.right.get)

        (url, eventList, secret, encryptKey, maxSize)

      }
      hooks.map(h => WebhookSettings(h._1, h._2, h._3, h._4, h._5))
    }
  }

  private def setEventSettings(config: Config): Either[ValidationError, WebhookEventSettings] = {
    val path = "rules"
    val afterH = config.getConfInt(s"$path.afterHeight", 0)
    val afterT = config.getConfInt(s"$path.afterTime", 0)

    config.as[String]("type") match {

      case "1" | "Block Appended" => 
        val withTxs = config.getConfBoolean(s"$path.withTxs", false)
        Right(BlockAppendedEventSettings(withTxs, afterH, afterT))

      case "2" | "Tx Confirmed" =>
        val acc = config.getConfSeqString(s"$path.relatedAccount", null)
        val inTypes = config.getConfSeqInt(s"$path.includeTypes", null)
        val exTypes = config.getConfSeqInt(s"$path.excludeTypes", null)
        val gteVal = config.getConfLong(s"$path.amount.gte", 0)
        val gtVal = config.getConfLong(s"$path.amount.gt", 0)
        val lteVal = config.getConfLong(s"$path.amount.lte", 6*10^9*10^8L)
        val ltVal = config.getConfLong(s"$path.amount.lt", 6*10^9*10^8L)
        val withFeeVal = config.getConfBoolean(s"$path.amount.withFee", false)
        
        Right(TxConfirmedEventSettings(acc, afterH, afterT, inTypes, exTypes, gteVal, gtVal, lteVal, ltVal, withFeeVal))

      case "3" | "State Updated" =>
        val acc = config.getConfSeqString(s"$path.relatedAccount", null)
        Right(StateUpdatedEventSettings(acc, afterH, afterT))

      case "4" | "Block Rollback" => 
        val acc = config.getConfSeqString(s"$path.relatedAccount", null)
        val wTxsOfTypes = config.getConfSeqInt(s"$path.withTxsOfTypes", null)
        val wTxsOfAccs = config.getConfSeqString(s"$path.withTxsOfAccs", null)
        val wStateOfAccs = config.getConfSeqString(s"$path.withStateOfAccs", null)

        Right(BlockRollbackEventSettings(acc, afterH, afterT, wTxsOfTypes, wTxsOfAccs, wStateOfAccs))
        
      case _ => 
        Left(ValidationError.InvalidEventTypeError)
      
    }
  }

  
}