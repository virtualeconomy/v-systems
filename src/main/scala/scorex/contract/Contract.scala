package scorex.contract

import com.wavesplatform.state2.ByteStr
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.ValidationError

sealed trait Contract {

  lazy val stringRepr: String = Contract.Prefix + ":" + name
  lazy val bytes: ByteStr = ByteStr(BytesSerializable.arrayWithSize(name.getBytes("UTF-8")) ++ content.getBytes("UTF-8"))

  val name: String
  val content: String
}

object Contract {

  val Prefix: String = "contract:"

  def buildContract(content: String, name: String): Either[ValidationError, Contract] = {
    case class ContractImpl(content: String, name: String) extends Contract
    Right(ContractImpl(content, name))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Contract] = {
    val (nameBytes, nameEnd) = Deser.parseArraySize(bytes, 0)
    buildContract(new String(bytes.slice(nameEnd, bytes.length), "UTF-8"), new String(nameBytes, "UTF-8"))
  }
}
