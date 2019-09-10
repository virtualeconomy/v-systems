package vsys.events

import vsys.utils.ScorexLogging
import vsys.settings.{WebhookEventSettings, BlockAppendedEventSettings}
import vsys.settings.WebhookEventRules
import vsys.blockchain.transaction.ProcessedTransaction
import vsys.blockchain.state.BlockDiff
import vsys.account.Address

class EventTrigger(url: String, secretKey: Option[String], encryptKey: Option[String], maxSize: Option[Int])

object EventTrigger extends ScorexLogging {

  def apply(url: String, secretKey: Option[String], encryptKey: Option[String], maxSize: Option[Int]) {
    new EventTrigger(url, secretKey, encryptKey, maxSize)
  }

  // This evoke should only handle type 1 & 2 events
  def evoke(webhookEventSetting: WebhookEventSettings, blockDiff: BlockDiff): Unit = {
    
    webhookEventSetting match {
      case e: BlockAppendedEventSettings =>
        val re = checkRules(e.eventRules, blockDiff);
        println(re.map(aList => aList._2.status.toString))
        
      case _ => log.error("Using Wrong Evoke For The Trigger")
    }
  }

  private[events] def checkRules(rules: Seq[WebhookEventRules], blockDiff: BlockDiff): List[(Int, ProcessedTransaction, Set[Address])] = {
    rules.foldLeft(blockDiff.txsDiff.transactions.toList)((accum, rule) =>
      accum.filter(aList => rule.applyRule(aList._2._1.toLong, aList._2._2, aList._2._3)))
    .collect {case (id, (h, tx, accs)) => (h, tx, accs)}
  }
}