package vsys.blockchain.contract

object DataType extends Enumeration {
  val PublicKey = Value(1)
  val Address = Value(2)
  val Amount = Value(3)
  val Int32 = Value(4)
  val ShortText = Value(5)
  val ContractAccount = Value(6)
  val Account = Value(7)

  def fromByte(b: Byte): Option[DataType.Value] = {
    if (b < DataType.PublicKey.id || b > DataType.Account.id)
      None
    else
      Some(DataType(b))
  }

  private def check(a: Byte, b: Byte): Boolean = {
    if (a == b) true
    else if (a == DataType.Account.id) b == DataType.Address.id || b == DataType.ContractAccount.id
    else if (b == DataType.Account.id) check(b, a)
    else false
  }

  def checkTypes(paraTypes: Array[Byte], dataTypes: Array[Byte]): Boolean = {
    paraTypes.length == dataTypes.length && (paraTypes, dataTypes).zipped.forall { case (a, b) => check(a, b) }
  }

}
