package vsys.contract

import com.google.common.primitives.Ints
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidContract

import scala.util.Success

sealed trait Contract {

  lazy val stringRepr: String = Contract.Prefix + Base58.encode(languageCode) + ":" + Base58.encode(languageVersion)
  lazy val bytes: ByteStr = ByteStr(languageCode ++ languageVersion ++ Deser.serializeArray(initializer)
    ++ Deser.serializeArrays(descriptor))

  val initializer: Array[Byte]
  val descriptor: Seq[Array[Byte]]
  val languageCode: Array[Byte]
  val languageVersion: Array[Byte]

  lazy val json: JsObject = Json.obj(
    "languageCode" -> Ints.fromByteArray(languageCode),
    "languageVersion" -> Ints.fromByteArray(languageVersion),
    "initializer" -> Base58.encode(initializer),
    "descriptor" -> Base58.encode(Deser.serializeArrays(descriptor))
  )
}

object Contract {

  val Prefix: String = "contract:"

  val MinContractByteSize = 8
  val MinContractStringSize: Int = base58Length(MinContractByteSize)

  def buildContract(languageCode: Array[Byte], languageVersion: Array[Byte], initializer: Array[Byte],
                    descriptor: Seq[Array[Byte]]): Either[ValidationError, Contract] = {
    case class ContractImpl(languageCode: Array[Byte], languageVersion: Array[Byte], initializer: Array[Byte],
                            descriptor: Seq[Array[Byte]]) extends Contract
    Right(ContractImpl(languageCode, languageVersion, initializer, descriptor))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Contract] = {
    if (isByteArrayValid(bytes)) {
      val languageCode = bytes.slice(0, 0 + 4)
      val languageVersion = bytes.slice(4 + 0, 4 + 4)
      val (initializer, initializerEnd) = Deser.parseArraySize(bytes, 4 + 4)
      val descriptor = Deser.parseArrays(bytes.slice(initializerEnd, bytes.length))
      buildContract(languageCode, languageVersion, initializer, descriptor)
    } else {
      Left(InvalidContract)
    }
  }

  def fromBase58String(base58String: String): Either[ValidationError, Contract] = {
    if (base58String.length < MinContractStringSize) Left(InvalidContract)
    else {
      Base58.decode(base58String) match {
        case Success(byteArray) if isByteArrayValid(byteArray) => Right(fromBytes(byteArray).right.get)
        case _ => Left(InvalidContract)
      }
    }
  }

  private def isByteArrayValid(bytes: Array[Byte]): Boolean = {
    bytes.length >= MinContractByteSize
  }
}
