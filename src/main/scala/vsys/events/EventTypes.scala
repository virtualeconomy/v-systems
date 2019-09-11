package vsys.events

import vsys.account.Address
import vsys.blockchain.transaction.ProcessedTransaction

trait Event

case class BlockAppendedEvent(
	url: String,
	secretKey: Option[String],
	encryptKey: Option[String],
	maxSize: Option[Int],
	eventData: List[(Int, ProcessedTransaction, Set[Address])]
) extends Event