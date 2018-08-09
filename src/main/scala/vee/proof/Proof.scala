package vee.proof

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.ValidationError
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl

sealed trait Proof {

  lazy val bytes: ByteStr = ByteStr(BytesSerializable.arrayWithSize(pkBytes) ++
    Array(proofType.id.asInstanceOf[Byte]) ++
    signature.arr
  )

  private lazy val (pkBytes, pkAddress): (Array[Byte], String) = pk match {
    case Some(key) => (key.publicKey, PublicKeyAccount.toAddress(key).address)
    case None => (Array.emptyByteArray, "None")
  }

  val pk: Option[PublicKeyAccount]
  val proofType: ProofType.Value
  val signature: ByteStr

  lazy val json: JsObject = Json.obj(
    "proofType" -> proofType,
    "publicKey" -> pkAddress,
    "signature" -> signature.base58
  )
}

object Proof {

  case class ProofImpl(pk: Option[PublicKeyAccount], proofType: ProofType.Value, signature: ByteStr) extends Proof

  def createProof(toSign: Array[Byte], signer: PrivateKeyAccount): Either[ValidationError, Proof] = {
    Right(ProofImpl(Option(PublicKeyAccount(signer.publicKey)), ProofType.Curve25519, ByteStr(EllipticCurveImpl.sign(signer, toSign)) ))
  }

  def buildProof(pk: Option[PublicKeyAccount], proofType: ProofType.Value, signature: ByteStr): Either[ValidationError, Proof] = {
    Right(ProofImpl(pk, proofType, signature))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Proof] = {
    if (bytes.length > 1)
      fromBytesWithValidLength(bytes)
    else
      Left(ValidationError.InvalidProofLength)
  }

  def fromBytesWithValidLength(bytes: Array[Byte]): Either[ValidationError, Proof] = {
    val (pkBytes, pkEnd) = Deser.parseArraySize(bytes, 0)
    val getPk: Option[PublicKeyAccount] = pkBytes.length match {
      case 0 => None
      case _ => Option(PublicKeyAccount(pkBytes))
    }
    bytes.slice(pkEnd, pkEnd + 1).headOption match {
      case None =>
        Left (ValidationError.InvalidProofBytes)
      case Some (b) =>
        ProofType.fromByte(b) match {
          case None =>
            Left (ValidationError.InvalidProofType)
          case Some(proofType) =>
            buildProof (
              getPk,
              proofType,
              ByteStr (bytes.slice (pkEnd + 1, bytes.length) )
            )
        }
    }
  }
}
