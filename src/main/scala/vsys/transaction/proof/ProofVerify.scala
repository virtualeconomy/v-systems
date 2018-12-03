package vsys.transaction.proof

object ProofVerify {

  def verifyProof(toVerify: Array[Byte], data: Array[Byte]): Boolean =
    data.head match {
      case txType: Byte if txType == ProofType.Curve25519.id =>
        EllipticCurve25519Proof.verifyProof(toVerify, data)

      case _ => false
    }
}
