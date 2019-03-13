package vsys.contract

object DataType extends Enumeration {
  val PublicKeyAccount = Value(1)
  val Address = Value(2)
  val Amount = Value(3)
  val Index = Value(4)
  val Description = Value(5)

  def fromByte(b: Byte): Option[DataType.Value] = {
    if (b < DataType.PublicKeyAccount.id || b > DataType.Description.id)
      None
    else
      Some(DataType(b))
  }
}
