package vsys.contract

import com.wavesplatform.state2.ByteStr
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.ValidationError

sealed trait Contract {

  lazy val stringRepr: String = Contract.Prefix + ":" + name
  lazy val bytes: ByteStr = ByteStr(BytesSerializable.arrayWithSize(name.getBytes("UTF-8")) ++
    (if (enabled) Array(1: Byte) else Array(0: Byte)) ++
    content.getBytes("UTF-8"))

  val name: String
  val content: String
  val enabled: Boolean
}

object Contract {

  val Prefix: String = "contract:"

  def buildContract(content: String, name: String, enabled: Boolean): Either[ValidationError, Contract] = {
    case class ContractImpl(content: String, name: String, enabled: Boolean) extends Contract
    Right(ContractImpl(content, name, enabled))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Contract] = {
    val (nameBytes, nameEnd) = Deser.parseArraySize(bytes, 0)

    val enabled= bytes.slice(nameEnd, nameEnd + 1).head == (1: Byte)
    buildContract(new String(bytes.slice(nameEnd+1, bytes.length), "UTF-8"), new String(nameBytes, "UTF-8"), enabled)
  }
}
