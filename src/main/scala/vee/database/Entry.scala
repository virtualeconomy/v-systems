package vee.database

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.serialization.Deser
import scorex.transaction.ValidationError

sealed trait Entry {

  lazy val bytes: ByteStr = ByteStr(Array(dataType.id.asInstanceOf[Byte]) ++
    Deser.serilizeString(data))

  val data: String
  val dataType: DataType.Value

  lazy val json: JsObject = Json.obj(
    "data"->data, "type"->dataType
  )
}

object Entry {

  val maxLength: Int= 16384 //16k, if this one >=shorts.max 32767, serilization can be a problem
  lazy val empty = Entry.buildEntry("", DataType.ByteArray).toOption.get

  def buildEntry(data: String, dataType: DataType.Value): Either[ValidationError, Entry] = {
    case class EntryImpl(data: String, dataType: DataType.Value) extends Entry
    if(data != null && data.length > maxLength)
      Left(ValidationError.TooLongDbEntry(data.length, maxLength))
    else if (dataType == DataType.ByteArray && !Deser.validUTF8(data))
      Left(ValidationError.InvalidUTF8String("dbEntry"))
    else
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
          Deser.deserilizeString(bytes.slice (1, bytes.length)),
          dataType
        )
    }
  }
}
