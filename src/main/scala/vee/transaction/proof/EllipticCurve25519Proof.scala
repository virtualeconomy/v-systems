package vee.transaction.proof

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.transaction.ValidationError
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.KeyLength

sealed trait EllipticCurve25519Proof extends Proof{

  lazy val bytes: ByteStr = ByteStr(Array(proofType.id.asInstanceOf[Byte]) ++
    publicKey.publicKey ++
    signature.arr
  )

  val proofType: ProofType.Value = ProofType.Curve25519
  val publicKey: PublicKeyAccount
  val signature: ByteStr

  lazy val json: JsObject = Json.obj(
    "proofType" -> proofType, // we can also write the detail name here
    "publicKey" -> Base58.encode(publicKey.publicKey),
    "signature" -> signature.base58
  )

}

object EllipticCurve25519Proof {

  private case class EllipticCurve25519ProofImpl(publicKey: PublicKeyAccount, signature: ByteStr) extends EllipticCurve25519Proof

  def createProof(toSign: Array[Byte], signer: PrivateKeyAccount): EllipticCurve25519Proof = {
    EllipticCurve25519ProofImpl(PublicKeyAccount(signer.publicKey), ByteStr(EllipticCurveImpl.sign(signer, toSign)) )
  }

  def verifyProof(toSign: Array[Byte], bytes: Array[Byte]): Boolean = {
    fromBytes(bytes) match {
      case Left(l) => false
      case Right(r) => EllipticCurveImpl.verify(r.signature.arr, toSign, r.publicKey.publicKey)
    }
  }

  def buildProof(publicKey: PublicKeyAccount, signature: ByteStr): EllipticCurve25519Proof = {
    EllipticCurve25519ProofImpl(publicKey, signature)
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, EllipticCurve25519Proof] = {
    if (bytes.length >= KeyLength + 1)
      fromBytesWithValidLength(bytes)
    else
      Left(ValidationError.InvalidProofLength)
  }

  def fromBytesWithValidLength(bytes: Array[Byte]): Either[ValidationError, EllipticCurve25519Proof] = {
    val publicKey = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
    val signature = ByteStr(bytes.slice(KeyLength + 1, bytes.length))
    bytes.slice(0, 1).headOption match {
      case None =>
        Left (ValidationError.InvalidProofBytes)
      case Some (b) =>
        ProofType.fromByte(b) match {
          case Some(proofType) if proofType == ProofType.Curve25519 =>
            Right(buildProof(publicKey, signature))
          case _ =>
            Left (ValidationError.InvalidProofType)
        }
    }
  }
}
