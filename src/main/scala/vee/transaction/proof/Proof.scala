package vee.transaction.proof

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.JsObject

sealed trait Proof {

  val bytes: ByteStr
  val proofType: ProofType.Value
  val json: JsObject

}

object Proof {
  case class ProofImpl(bytes: ByteStr, proofType: ProofType.Value, json: JsObject) extends Proof

  def toProof(bytes: ByteStr, proofType: ProofType.Value, json: JsObject): Proof = {
    ProofImpl(bytes, proofType, json)
  }

  def verifyProof(toVerify: Array[Byte], proof: Proof): Boolean = {
    proof.bytes.arr.headOption match {
      case None => false
      case Some(b) =>
        ProofType.fromByte(b) match {
          case Some(proofType) if proofType == ProofType.Curve25519 =>
            EllipticCurve25519Proof.verifyProof(toVerify, proof.bytes.arr)
          case _ => false
        }
    }
  }
}

