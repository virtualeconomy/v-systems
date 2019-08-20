package vsys.settings

 trait WebhookEventSettings {
 	val typeId: Int
 	val typeDescription: String
 }

case class BlockAppendedEventSettings(withTxs: Boolean, 
							  		 afterHeight: Long, 
							         afterTime: Long) extends WebhookEventSettings {
	override val typeId 		 = 1
	override val typeDescription = "Block Appended"
}

case class TxConfirmedEventSettings(relatedAcc: Seq[String],
									afterHeight: Long,
									afterTime: Long,
									includeTypes: Seq[Int],
									excludeTypes: Seq[Int],
									amtGTE: Long, 			//amount greater or equal to 
									amtGT: Long,  			//grater 
									amtLTE: Long, 	//less than or equal to
									amtLT: Long,
									withFee: Boolean) extends WebhookEventSettings {

	override val typeId 		 = 2
	override val typeDescription = "Transaction Confirmed"
}

case class StateUpdatedEventSettings(relatedAcc: Seq[String],
									 afterHeight: Long,
									 afterTime: Long) extends WebhookEventSettings {

	override val typeId 		 = 3
	override val typeDescription = "State Updated"
}

case class BlockRollbackEventSettings(relatedAcc: Seq[String],
								 afterHeight: Long,
								 afterTime: Long,
								 withTxsOfTypes: Seq[Int],
								 withTxsOfAccs: Seq[String],
								 withStateOfAccs: Seq[String]) extends WebhookEventSettings {

	override val typeId 		 = 4
	override val typeDescription = "Block Rollback"
}
