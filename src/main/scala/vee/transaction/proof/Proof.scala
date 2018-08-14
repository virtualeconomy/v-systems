package vee.transaction.proof

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.ValidationError
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl

sealed trait Proof {

  lazy val bytes: ByteStr = ByteStr(BytesSerializable.arrayWithSize(publicKeyBytes) ++
    Array(proofType.id.asInstanceOf[Byte]) ++
    signature.arr
  )

  private lazy val (publicKeyBytes, publicKeyAddress): (Array[Byte], String) = publicKey match {
    case Some(key) => (key.publicKey, PublicKeyAccount.toAddress(key).address)
    case None => (Array.emptyByteArray, "None")
  }

  val publicKey: Option[PublicKeyAccount]
  val proofType: ProofType.Value
  val signature: ByteStr

  lazy val json: JsObject = Json.obj(
    "proofType" -> proofType,
    "publicKey" -> publicKeyAddress,
    "signature" -> signature.base58
  )
}

object Proof {

  case class ProofImpl(publicKey: Option[PublicKeyAccount], proofType: ProofType.Value, signature: ByteStr) extends Proof

  def createProof(toSign: Array[Byte], signer: PrivateKeyAccount): Either[ValidationError, Proof] = {
    Right(ProofImpl(Option(PublicKeyAccount(signer.publicKey)), ProofType.Curve25519, ByteStr(EllipticCurveImpl.sign(signer, toSign)) ))
  }

  def buildProof(publickKey: Option[PublicKeyAccount], proofType: ProofType.Value, signature: ByteStr): Either[ValidationError, Proof] = {
    Right(ProofImpl(publickKey, proofType, signature))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Proof] = {
    if (bytes.length > 1)
      fromBytesWithValidLength(bytes)
    else
      Left(ValidationError.InvalidProofLength)
  }

  def fromBytesWithValidLength(bytes: Array[Byte]): Either[ValidationError, Proof] = {
    val (publicKeyBytes, publicKeyEnd) = Deser.parseArraySize(bytes, 0)
    val getPublicKey: Option[PublicKeyAccount] = publicKeyBytes.length match {
      case 0 => None
      case _ => Option(PublicKeyAccount(publicKeyBytes))
    }
    bytes.slice(publicKeyEnd, publicKeyEnd + 1).headOption match {
      case None =>
        Left (ValidationError.InvalidProofBytes)
      case Some (b) =>
        ProofType.fromByte(b) match {
          case None =>
            Left (ValidationError.InvalidProofType)
          case Some(proofType) =>
            buildProof (
              getPublicKey,
              proofType,
              ByteStr (bytes.slice (publicKeyEnd + 1, bytes.length) )
            )
        }
    }
  }
}
