package vsys.contract

object DataType extends Enumeration {
  val PublicKeyAccount = Value(1)
  val Address = Value(2)
  val Amount = Value(3)

  def fromByte(b: Byte): Option[DataType.Value] = {
    if (b < DataType.PublicKeyAccount.id || b > DataType.Amount.id)
      None
    else
      Some(DataType(b))
  }
}
