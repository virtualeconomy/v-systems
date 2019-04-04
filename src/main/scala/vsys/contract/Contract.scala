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
    ++ Deser.serializeArray(Deser.serializeArrays(descriptor))
    ++ Deser.serializeArray(Deser.serializeArrays(stateVar)) ++ Deser.serializeArrays(texture))

  val initializer: Array[Byte]
  val descriptor: Seq[Array[Byte]]
  val stateVar: Seq[Array[Byte]]
  val texture: Seq[Array[Byte]]
  val languageCode: Array[Byte]
  val languageVersion: Array[Byte]

  lazy val json: JsObject = Json.obj(
    "languageCode" -> Ints.fromByteArray(languageCode),
    "languageVersion" -> Ints.fromByteArray(languageVersion),
    "initializer" -> Base58.encode(initializer),
    "descriptor" -> Base58.encode(Deser.serializeArrays(descriptor)),
    "stateVar" -> Base58.encode(Deser.serializeArrays(stateVar)),
    "texture" -> Base58.encode(Deser.serializeArrays(texture))
  )
}

object Contract {

  val Prefix: String = "contract:"

  val MinContractByteSize = 8
  val MinContractStringSize: Int = base58Length(MinContractByteSize)
  val LanguageCodeByteLength = 4
  val LanguageVersionByteLength = 4

  def buildContract(languageCode: Array[Byte], languageVersion: Array[Byte],
                    initializer: Array[Byte], descriptor: Seq[Array[Byte]],
                    stateVar: Seq[Array[Byte]], texture: Seq[Array[Byte]]): Either[ValidationError, Contract] = {
    case class ContractImpl(languageCode: Array[Byte], languageVersion: Array[Byte],
                            initializer: Array[Byte], descriptor: Seq[Array[Byte]],
                            stateVar: Seq[Array[Byte]], texture: Seq[Array[Byte]]) extends Contract
    Right(ContractImpl(languageCode, languageVersion, initializer, descriptor, stateVar, texture))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Contract] = {
    if (isByteArrayValid(bytes)) {
      val languageCode = bytes.slice(0, LanguageCodeByteLength)
      val languageVersion = bytes.slice(LanguageCodeByteLength, LanguageCodeByteLength + LanguageVersionByteLength)
      val (initializer, initializerEnd) = Deser.parseArraySize(bytes, LanguageCodeByteLength + LanguageVersionByteLength)
      val (descriptorBytes, descriptorEnd) = Deser.parseArraySize(bytes, initializerEnd)
      val descriptor = Deser.parseArrays(descriptorBytes)
      val (stateVarBytes, stateVarEnd) = Deser.parseArraySize(bytes, descriptorEnd)
      val stateVar = Deser.parseArrays(stateVarBytes)
      val texture = Deser.parseArrays(bytes.slice(stateVarEnd, bytes.length))
      buildContract(languageCode, languageVersion, initializer, descriptor, stateVar, texture)
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

  def checkStateVar(stateVar: Array[Byte], dataType: DataType.Value): Boolean =
    stateVar.length == 2 && dataType == DataType(stateVar(1))

  private def isByteArrayValid(bytes: Array[Byte]): Boolean = {
    bytes.length >= MinContractByteSize
  }
}
