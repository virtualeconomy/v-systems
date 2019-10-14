package vsys.events

import vsys.utils.ScorexLogging
import vsys.settings.{EventSettings, BlockAppendedEventSettings, TxConfirmedEventSettings, WebhookEventRules}
import vsys.blockchain.transaction.ProcessedTransaction
import vsys.blockchain.state.BlockDiff
import vsys.blockchain.block.Block
import vsys.account.Address
import akka.actor.ActorRef

class EventTriggers(eventWriter: ActorRef, eventSetting: EventSettings) extends ScorexLogging {

  // TO DO: Should handle more webhook event settings
  def evokeWebhook(block: Block, blockDiff: BlockDiff): Unit = {
    val webhookSettings = eventSetting.webhookSettings
    webhookSettings.map {webhookSetting =>
      val url = webhookSetting.url
      val scKey = webhookSetting.secretKey
      val enKey = webhookSetting.encryptKey
      val maxSize = webhookSetting.maxSize

      webhookSetting.events.map {webhookEventSetting =>
        webhookEventSetting match {
          case e: BlockAppendedEventSettings =>
            val re = checkRules(e.eventRules, block.timestamp, blockDiff)
            if (re.nonEmpty) {
              eventWriter ! BlockAppendedEvent(url, scKey, enKey, maxSize, re)
            }

          case e: TxConfirmedEventSettings =>
            val re = checkRules(e.eventRules, block.timestamp, blockDiff)
            eventWriter ! TxConfirmedEvent(url, scKey, enKey, maxSize, re)

          case _ => log.error("Using Wrong Evoke For The Trigger")
        }
      }
    }
  }

  private[events] def checkRules(rules: Seq[WebhookEventRules], blockTime: Long, blockDiff: BlockDiff): List[(Int, ProcessedTransaction, Set[Address])] = {
    rules.foldLeft(blockDiff.txsDiff.transactions.toList)((accum, rule) =>
      accum.filter(aList => aList match {case (id, (h, tx, accs)) => rule.applyRule(h.toLong, blockTime, tx, accs)}
      )).collect {case (id, (h, tx, accs)) => (h, tx, accs)}
  }
}

object EventTriggers {
  def apply(eventWriter: ActorRef, eventSetting: EventSettings): EventTriggers = {
    new EventTriggers(eventWriter, eventSetting)
  }
}