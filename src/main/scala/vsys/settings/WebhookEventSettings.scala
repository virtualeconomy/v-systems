package vsys.settings

import com.typesafe.config.Config
import vsys.blockchain.transaction.ValidationError

trait WebhookEventSettings {
  val typeId: Int 
  val typeDescription: String
  val eventRules: Seq[WebhookEventRules]
}

case class BlockAppendedEventSettings(override val eventRules: Seq[WebhookEventRules]) extends WebhookEventSettings {
  override val typeId = 1
  override val typeDescription = "Block Appended"
}

case class TxConfirmedEventSettings(override val eventRules: Seq[WebhookEventRules]) extends WebhookEventSettings {
  override val typeId = 2
  override val typeDescription = "Tx Confirmed"
}

case class StateUpdatedEventSettings(override val eventRules: Seq[WebhookEventRules]) extends WebhookEventSettings {
  override val typeId = 3
  override val typeDescription = "State Updated"
}

case class BlockRollbackEventSettings(override val eventRules: Seq[WebhookEventRules]) extends WebhookEventSettings {
  override val typeId = 4
  override val typeDescription = "Block Rollback"
}


object WebhookEventSettings {
  def apply(config: Config, typeId: String): Either[ValidationError, WebhookEventSettings] = typeId match {
    case "1" | "Block Appended" =>
      val ruleFields = Seq("afterHeight", "afterTime", "withTxs", "withMintingTxs")
      val ruleList = formWebhookEventRules(config, ruleFields)

      Right(BlockAppendedEventSettings(ruleList))

    case "2" | "Tx Confirmed" =>
      val ruleFields = Seq("relatedAccount", "afterHeight", "afterTime", "includeTypes", 
        "excludeTypes", "amount.gte", "amount.gt", "amount.lte", "amount.lt, amount.withFee")
      val ruleList = formWebhookEventRules(config, ruleFields)

      Right(TxConfirmedEventSettings(ruleList))

    case "3" | "State Updated" =>
      val ruleFields = Seq("afterTime", "afterHeight", "relatedAccount")
      val ruleList = formWebhookEventRules(config, ruleFields)

      Right(StateUpdatedEventSettings(ruleList))

    case "4" | "Block Rollback" =>
      val ruleFields = Seq("afterTime", "afterHeight", "withTxsOfTypes", "withTxsOfAccs", "withStateOfAccs")
      val ruleList = formWebhookEventRules(config, ruleFields)

      Right(BlockRollbackEventSettings(ruleList))

    case _ => Left(ValidationError.InvalidEventTypeError)
  }

  private def formWebhookEventRules(config: Config, ruleFields: Seq[String]): Seq[WebhookEventRules] = {
    val generator = WebhookEventRules(config)(_)
    ruleFields.map(aStr => generator(s"rules.$aStr")).filter(_.isRight).map(_.right.get)
  }
}