package vsys.transaction

object TransactionStatus extends Enumeration {
  val Unprocessed = Value(1)
  val Success = Value(2)
  val Failed = Value(3)
  val ContendFailed = Value(4)
  val RegisterContractFailed = Value(5)
  val ExecuteContractFunctionFailed = Value(6)
}
