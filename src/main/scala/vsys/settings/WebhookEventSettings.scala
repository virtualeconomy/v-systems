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
      val fields = Seq[EventConfigReader](AfterHeight, AfterTime, WithTxs, WithMintingTxs)
      
      Right(BlockAppendedEventSettings(fields.flatMap(_.fromConfig(config))))

    case "2" | "Tx Confirmed" =>
      val fields = Seq[EventConfigReader](RelatedAccs, AfterHeight, AfterTime, IncludeTypes, 
        ExcludeTypes, AmtGTE, AmtGT, AmtLTE, AmtLT, AmtWithFee)

      Right(TxConfirmedEventSettings(fields.flatMap(_.fromConfig(config))))

    case "3" | "State Updated" =>
      val fields = Seq[EventConfigReader](AfterTime, AfterHeight, RelatedAccs)

      Right(StateUpdatedEventSettings(fields.flatMap(_.fromConfig(config))))

    case "4" | "Block Rollback" =>
      val fields = Seq[EventConfigReader](AfterTime, AfterHeight, RelatedAccs, WithTxsOfTypes, WithTxsOfAccs, WithStateOfAccs)

      Right(BlockRollbackEventSettings(fields.flatMap(_.fromConfig(config))))

    case _ => Left(ValidationError.InvalidEventTypeError)
  }
}