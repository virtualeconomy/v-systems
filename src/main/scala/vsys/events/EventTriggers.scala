package vsys.events

import vsys.utils.ScorexLogging
import vsys.settings.{EventSettings, BlockAppendedEventSettings}
import vsys.settings.WebhookEventRules
import vsys.blockchain.transaction.ProcessedTransaction
import vsys.blockchain.state.BlockDiff
import vsys.account.Address
import akka.actor.ActorRef

class EventTrigger(eventWriter: ActorRef, eventSetting: EventSettings) extends ScorexLogging {

  // TO DO: Should handle more webhook event settings
  def evokeWebhook(blockDiff: BlockDiff): Unit = {
    val webhookSettings = eventSetting.webhookSettings
    webhookSettings.map {webhookSetting =>
      val url = webhookSetting.url
      val scKey = webhookSetting.secretKey
      val enKey = webhookSetting.encryptKey
      val maxSize = webhookSetting.maxSize

      webhookSetting.events.map {webhookEventSetting =>
        webhookEventSetting match {
          case e: BlockAppendedEventSettings =>
            val re = checkRules(e.eventRules, blockDiff);
            eventWriter ! BlockAppendedEvent(url, scKey, enKey, maxSize, re)

          case _ => log.error("Using Wrong Evoke For The Trigger")
        }
      }
    }
  }

  private[events] def checkRules(rules: Seq[WebhookEventRules], blockDiff: BlockDiff): List[(Int, ProcessedTransaction, Set[Address])] = {
    rules.foldLeft(blockDiff.txsDiff.transactions.toList)((accum, rule) =>
      accum.filter(aList => rule.applyRule(aList._2._1.toLong, aList._2._2, aList._2._3)))
    .collect {case (id, (h, tx, accs)) => (h, tx, accs)}
  }
}

object EventTrigger {
  def apply(eventWriter: ActorRef, eventSetting: EventSettings): EventTrigger = {
    new EventTrigger(eventWriter, eventSetting)
  }
}