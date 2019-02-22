package vsys.contract

import com.wavesplatform.state2.ByteStr
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.ValidationError

sealed trait Contract {

  lazy val stringRepr: String = Contract.Prefix + Base58.encode(languageCode) + ":" + Base58.encode(languageVersion)
  lazy val bytes: ByteStr = ByteStr(languageCode ++ languageVersion ++ Deser.serializeArrays(descriptor))

  val descriptor: Seq[Array[Byte]]
  val languageCode: Array[Byte]
  val languageVersion: Array[Byte]

}

object Contract {

  val Prefix: String = "contract:"

  def buildContract(languageCode: Array[Byte], languageVersion: Array[Byte], descriptor: Seq[Array[Byte]]):
  Either[ValidationError, Contract] = {
    case class ContractImpl(languageCode: Array[Byte], languageVersion: Array[Byte], descriptor: Seq[Array[Byte]])
      extends Contract
    Right(ContractImpl(languageCode, languageVersion, descriptor))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Contract] = {
    val languageCode = bytes.slice(0, 0 + 4)
    val languageVersion = bytes.slice(4 + 0, 4 + 4)
    val descriptor = Deser.parseArrays(bytes.slice(8 + 0, bytes.length))
    buildContract(languageCode, languageVersion, descriptor)
  }
}
