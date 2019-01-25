package vsys.contract

import play.api.libs.json.{JsObject, Json}
import scorex.transaction.ValidationError

case class DataEntry(data: Array[Byte],
                     dataType: DataType.Value) {

  lazy val bytes: Array[Byte] = Array(dataType.id.asInstanceOf[Byte]) ++ data

  lazy val json: JsObject = Json.obj(
    "data" -> data,
    "type" -> dataType
  )
}

object DataEntry {

  def create(data: Array[Byte], dataType: DataType.Value): Either[ValidationError, DataEntry] = {
    if (dataType.id < DataType.PublicKeyAccount.id || dataType.id > DataType.Amount.id) {
      Left(ValidationError.InvalidDataType)
    } else {
      Right(DataEntry(data, dataType))
    }
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, DataEntry] = {
    if (bytes.length > 0) {
      DataType.fromByte(bytes(0)) match {
        case None => Left(ValidationError.InvalidDataType)

        case Some(dataType) => create(
          bytes.slice(1, bytes.length),
          dataType)
      }
    } else {
      Left(ValidationError.InvalidDataLength)
    }
  }
}
