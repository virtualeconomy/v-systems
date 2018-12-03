package vsys.transaction.proof

object ProofType extends Enumeration {
  val Curve25519 = Value(1)
//  val NoType = Value(2)

  def fromByte(b: Byte): Option[ProofType.Value] = {
    if (b < ProofType.Curve25519.id || b > ProofType.Curve25519.id)
      None
    else
      Some(ProofType(b))
  }
}