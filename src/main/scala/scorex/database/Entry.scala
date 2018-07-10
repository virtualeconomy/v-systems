package scorex.database

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.transaction.ValidationError

sealed trait Entry {

  lazy val bytes: ByteStr = ByteStr(Array(dataType.id.asInstanceOf[Byte]) ++
    data.getBytes("UTF-8"))

  val data: String
  val dataType: DataType.Value

  lazy val json: JsObject = Json.obj(
    "data"->data, "type"->dataType
  )
}

object Entry {

  def buildEntry(data: String, dataType: DataType.Value): Either[ValidationError, Entry] = {
    case class EntryImpl(data: String, dataType: DataType.Value) extends Entry
    Right(EntryImpl(data, dataType))
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Entry] = {
    if (bytes.length > 0)
      fromBytesWithValidLength(bytes)
    else
      Left(ValidationError.InvalidDataLength)
  }

  def fromBytesWithValidLength(bytes: Array[Byte]): Either[ValidationError, Entry] = {
    DataType.fromByte(bytes(0)) match {
      case None =>
        Left (ValidationError.InvalidDataType)
      case Some (dataType) =>
        buildEntry (
          new String (bytes.slice (1, bytes.length), "UTF-8"),
          dataType
        )
    }
  }
}
