package vsys.events

import vsys.account.Address
import vsys.blockchain.transaction.ProcessedTransaction
import play.api.libs.json.JsObject

trait Event {
  val url: String
  val scKey: Option[String]
  val enKey: Option[String]
  val maxSize: Int
  val subscribeData: JsObject
}

case class BlockAppendedEvent(
  override val url: String,
  override val scKey: Option[String],
  override val enKey: Option[String],
  override val maxSize: Int,
  override val subscribeData: JsObject
) extends Event

case class TxConfirmedEvent(
  override val url: String,
  override val scKey: Option[String],
  override val enKey: Option[String],
  override val maxSize: Int,
  override val subscribeData: JsObject
) extends Event

case class StateUpdatedEvent(
  override val url: String,
  override val scKey: Option[String],
  override val enKey: Option[String],
  override val maxSize: Int,
  override val subscribeData: JsObject
) extends Event

case class BlockRollbackEvent(
  override val url: String,
  override val scKey: Option[String],
  override val enKey: Option[String],
  override val maxSize: Int,
  override val subscribeData: JsObject
) extends Event