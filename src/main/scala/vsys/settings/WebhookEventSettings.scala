package vsys.settings

trait WebhookEventSettings{
  val typeId: Int 
  val typeDescription: String
}

case class BlockAppendedEventSettings(
  withTxs: Boolean, 
  afterHeight: Long, 
  afterTime: Long
) extends WebhookEventSettings {
  override val typeId = 1
  override val typeDescription = "Block Appended"
}

case class TxConfirmedEventSettings(
  relatedAcc: Option[Seq[String]],
  afterHeight: Long,
  afterTime: Long,
  includeTypes: Option[Seq[Int]],
  excludeTypes: Option[Seq[Int]],
  amtGTE: Long,       //amount greater or equal to 
  amtGT: Long,        //grater 
  amtLTE: Long,       //less than or equal to
  amtLT: Long,
  withFee: Boolean
) extends WebhookEventSettings {
  override val typeId = 2
  override val typeDescription = "Tx Confirmed"
}


case class StateUpdatedEventSettings(
  relatedAcc: Option[Seq[String]],
  afterHeight: Long,
  afterTime: Long
) extends WebhookEventSettings {
  override val typeId = 3
  override val typeDescription = "State Updated"
}


case class BlockRollbackEventSettings(
  relatedAcc: Option[Seq[String]],
  afterHeight: Long,
  afterTime: Long,
  withTxsOfTypes: Option[Seq[Int]],
  withTxsOfAccs: Option[Seq[String]],
  withStateOfAccs: Option[Seq[String]]
) extends WebhookEventSettings {
  override val typeId = 4
  override val typeDescription = "Block Rollback"
}


object BlockAppendedEventSettings {

  def apply(withTxs: Boolean, afterHeight: Long, afterTime: Long): BlockAppendedEventSettings = {
    new BlockAppendedEventSettings(withTxs, afterHeight, afterTime)
  }
}

object TxConfirmedEventSettings {

  def apply(relatedAcc: Option[Seq[String]],
            afterHeight: Long, 
            afterTime: Long,
            includeTypes: Option[Seq[Int]], 
            excludeTypes: Option[Seq[Int]], 
            amtGTE: Long, 
            amtGT: Long, 
            amtLTE: Long, 
            amtLT: Long, 
            withFee: Boolean): TxConfirmedEventSettings = {

    new TxConfirmedEventSettings(relatedAcc, afterHeight, afterTime, includeTypes, excludeTypes, amtGTE, amtGT, amtLTE, amtLT, withFee)
  }
}

object StateUpdatedEventSettings {

  def apply(relatedAcc: Option[Seq[String]], afterHeight: Long, afterTime: Long): StateUpdatedEventSettings = {
    new StateUpdatedEventSettings(relatedAcc, afterHeight, afterTime)
  }
}

object BlockRollbackEventSettings {

  def apply(relatedAcc: Option[Seq[String]], 
            afterHeight: Long, 
            afterTime: Long,
            withTxsOfTypes: Option[Seq[Int]], 
            withTxsOfAccs: Option[Seq[String]], 
            withStateOfAccs: Option[Seq[String]]): BlockRollbackEventSettings = {
    new BlockRollbackEventSettings(relatedAcc, afterHeight, afterTime, withTxsOfTypes, withTxsOfAccs, withStateOfAccs)
  }
}
