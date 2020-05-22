package vsys.blockchain.contract

object CallType extends Enumeration {
  val Trigger = Value(1)
  val Function = Value(2)

  def fromByte(b: Byte): Option[CallType.Value] = {
    if (b < CallType.Trigger.id || b > CallType.Function.id)
      None
    else
      Some(CallType(b))
  }

}