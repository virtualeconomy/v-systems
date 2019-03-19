package vsys.contract

object DataType extends Enumeration {
  val PublicKey = Value(1)
  val Address = Value(2)
  val Amount = Value(3)
  val Int32 = Value(4)
  val ShortText = Value(5)
  val ContractAccount = Value(6)
  val Account = Value(7)

  def fromByte(b: Byte): Option[DataType.Value] = {
    if (b < DataType.PublicKey.id || b > DataType.ContractAccount.id)
      None
    else
      Some(DataType(b))
  }

  def check(a: DataType.Value, b: DataType.Value): Boolean = {
    if (a == b) true
    else if (a == DataType.Account) b == DataType.Address || b == DataType.ContractAccount
    else if (b == DataType.Account) check(b, a)
    else false
  }

}
