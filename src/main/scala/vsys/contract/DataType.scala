package vsys.contract

object DataType extends Enumeration {
  val PublicKey = Value(1)
  val Address = Value(2)
  val Amount = Value(3)
  val Int32 = Value(4)
  val ShortText = Value(5)
  val Account = Value(6)

  def fromByte(b: Byte): Option[DataType.Value] = {
    if (b < DataType.PublicKey.id || b > DataType.Account.id)
      None
    else
      Some(DataType(b))
  }
}
