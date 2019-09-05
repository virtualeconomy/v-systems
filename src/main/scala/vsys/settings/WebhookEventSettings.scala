package vsys.settings

import com.typesafe.config.Config
import vsys.utils.ScorexLogging

trait EventConfigReader {
  val rules: Seq[RuleConfigReader]
  def getRules(config: Config): Seq[WebhookEventRules] = {
    rules.flatMap(_.fromConfig(config))
  }
}

trait WebhookEventSettings {
  val typeId: Int
  val typeDescription: String
  val eventRules: Seq[WebhookEventRules]
}

object WebhookEventSettings extends ScorexLogging {
  def apply(config: Config, typeId: String): Either[Unit, WebhookEventSettings] = typeId match {
    case "1" | "Block Appended" => Right(BlockAppendedEventSettings.fromConfig(config))
    case "2" | "Tx Confirmed" => Right(TxConfirmedEventSettings.fromConfig(config))
    case "3" | "State Updated" => Right(StateUpdatedEventSettings.fromConfig(config))
    case "4" | "Block Rollback" => Right(BlockRollbackEventSettings.fromConfig(config))
    case _ => Left(log.error("Event Type In Config vsys.Event.webHooks.events Is Invalid !!!"))
  }
}

case class BlockAppendedEventSettings(override val eventRules: Seq[WebhookEventRules]) extends WebhookEventSettings {
  override val typeId = 1
  override val typeDescription = "Block Appended"
}

object BlockAppendedEventSettings extends EventConfigReader {
  override val rules = Seq[RuleConfigReader](WithTxs, WithMintingTxs, AfterHeight, AfterTime)

  def fromConfig(config: Config): WebhookEventSettings = {
    BlockAppendedEventSettings(getRules(config))
  }
}

case class TxConfirmedEventSettings(override val eventRules: Seq[WebhookEventRules]) extends WebhookEventSettings {
  override val typeId = 2
  override val typeDescription = "Tx Confirmed"
}

object TxConfirmedEventSettings extends EventConfigReader {
  override val rules = Seq[RuleConfigReader](IncludeTypes, ExcludeTypes, RelatedAccs, AfterHeight, AfterTime,
    AmtGTE, AmtGT, AmtLTE, AmtLT, AmtWithFee)

  def fromConfig(config: Config): WebhookEventSettings = {
    TxConfirmedEventSettings(getRules(config))
  }
}

case class StateUpdatedEventSettings(override val eventRules: Seq[WebhookEventRules]) extends WebhookEventSettings {
  override val typeId = 3
  override val typeDescription = "State Updated"
}

object StateUpdatedEventSettings extends EventConfigReader {
  override val rules = Seq[RuleConfigReader](AfterTime, AfterHeight, RelatedAccs)

  def fromConfig(config: Config): WebhookEventSettings = {
    StateUpdatedEventSettings(getRules(config))
  }
}

case class BlockRollbackEventSettings(override val eventRules: Seq[WebhookEventRules]) extends WebhookEventSettings {
  override val typeId = 4
  override val typeDescription = "Block Rollback"
}

object BlockRollbackEventSettings extends EventConfigReader {
  override val rules = Seq[RuleConfigReader](AfterTime, AfterHeight, RelatedAccs,
    WithTxsOfTypes, WithTxsOfAccs, WithStateOfAccs)

  def fromConfig(config: Config): WebhookEventSettings = {
    BlockRollbackEventSettings(getRules(config))
  }
}