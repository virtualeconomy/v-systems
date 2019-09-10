package vsys.events

trait Event
case class BlockAppendedEvent(url: String, secretKey: Option[String], encryptKey: Option[String], maxSize: Option[Int]) extends Event