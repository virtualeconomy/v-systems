package vsys.events

import vsys.blockchain.block.Block
import vsys.utils.ScorexLogging
import vsys.settings.{WebhookEventSettings, BlockAppendedEventSettings}
import vsys.settings.{AfterHeight, AfterTime, WithTxs, WithMintingTxs}
import vsys.settings.WebhookEventRules
import vsys.blockchain.transaction.ProcessedTransaction

class EventTrigger(url: String, secretKey: Option[String], encryptKey: Option[String], maxSize: Option[Int])

object EventTrigger extends ScorexLogging {

  def apply(url: String, secretKey: Option[String], encryptKey: Option[String], maxSize: Option[Int]) {
    new EventTrigger(url, secretKey, encryptKey, maxSize)
  }

  // This evoke should only handle type 1 & 2 events
  def evoke(block: Block, height: Long, webhookEventSetting: WebhookEventSettings): Unit = {
    webhookEventSetting match {
      case e: BlockAppendedEventSettings =>
        val re = checkRules(e.eventRules, block.transactionData, height);
        re.map(tx => log.info(tx.status.toString))
      case _ => log.error("Using Wrong Evoke For The Trigger")
    }
  }

  private[events] def checkRules(rules: Seq[WebhookEventRules], txs: Seq[ProcessedTransaction], height: Long): Seq[ProcessedTransaction] = {
    rules.foldLeft(txs)((accum, rule) => accum.filter(tx => rule match {
      case r: AfterHeight => r.applyRule(height)
      case r: AfterTime => r.applyRule(tx.transaction.timestamp)
      case r: WithTxs => r.applyRule()
      case r: WithMintingTxs => r.applyRule(tx.transaction.transactionType)
      case _ => true
    }))
  }
}