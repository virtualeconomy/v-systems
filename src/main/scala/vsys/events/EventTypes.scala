package vsys.events

import vsys.account.Address
import vsys.blockchain.transaction.ProcessedTransaction

trait Event

case class BlockAppendedEvent(
  url: String,
  secretKey: Option[String],
  encryptKey: Option[String],
  maxSizeIn: Option[Int],
  eventDataIn: List[(Int, ProcessedTransaction, Set[Address])]) extends Event

case class TxConfirmedEvent(
  urlIn: String,
  scKey: Option[String],
  enKey: Option[String],
  maxSizeIn: Option[Int],
  eventDataIn: List[(Int, ProcessedTransaction, Set[Address])]) extends Event