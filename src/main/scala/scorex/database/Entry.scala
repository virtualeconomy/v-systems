package scorex.database

import com.wavesplatform.state2.ByteStr
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.ValidationError

sealed trait Entry {

  lazy val stringRepr: String = name
  lazy val bytes: ByteStr = ByteStr(BytesSerializable.arrayWithSize(name.getBytes("UTF-8")) ++
    Array(dataType.id.asInstanceOf[Byte]) ++
    data.getBytes("UTF-8"))

  val name: String
  val data: String
  val dataType: DataType.Value
}

object Entry {

  def buildEntry(data: String, name: String, dataType: DataType.Value): Either[ValidationError, Entry] = {
    case class EntryImpl(data: String, name: String, dataType: DataType.Value) extends Entry
    Right(EntryImpl(data, name, dataType))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Entry] = {
    val (nameBytes, nameEnd) = Deser.parseArraySize(bytes, 0)

    val dataTypeValue = bytes.slice(nameEnd, nameEnd + 1).head
    if (dataTypeValue < DataType.ByteArray.id || dataTypeValue > DataType.NoType.id)
      Left(ValidationError.InvalidDataType)
    else
      buildEntry(
        new String(bytes.slice(nameEnd+1, bytes.length), "UTF-8"),
        new String(nameBytes, "UTF-8"),
        DataType(dataTypeValue)
      )
  }
}
