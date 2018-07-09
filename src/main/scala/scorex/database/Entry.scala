package scorex.database

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
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

  lazy val json: JsObject = Json.obj(
    "name" -> name, "data"->data, "type"->dataType
  )
}

object Entry {

  def buildEntry(data: String, name: String, dataType: DataType.Value): Either[ValidationError, Entry] = {
    case class EntryImpl(data: String, name: String, dataType: DataType.Value) extends Entry
    Right(EntryImpl(data, name, dataType))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Entry] = {
    if (bytes.length > 1)
      fromBytesWithValidLength(bytes)
    else
      Left(ValidationError.InvalidDataLength)
  }

  def fromBytesWithValidLength(bytes: Array[Byte]): Either[ValidationError, Entry] = {
    val (nameBytes, nameEnd) = Deser.parseArraySize(bytes, 0)
    bytes.slice(nameEnd, nameEnd + 1).headOption match {
      case None =>
        Left(ValidationError.InvalidDataEntry)
      case Some(b) =>
        DataType.fromByte(b) match {
          case None =>
            Left (ValidationError.InvalidDataType)
          case Some (dataType) =>
            buildEntry (
              new String (bytes.slice (nameEnd + 1, bytes.length), "UTF-8"),
              new String (nameBytes, "UTF-8"),
              dataType
            )
        }
    }
  }
}
