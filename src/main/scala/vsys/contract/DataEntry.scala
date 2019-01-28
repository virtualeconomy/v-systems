package vsys.contract

import play.api.libs.json.{JsObject, Json}
import scorex.account.Address
import scorex.transaction.TransactionParser.KeyLength
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
        case Some(DataType.PublicKeyAccount) => if (bytes.length < 1 + KeyLength) {
          Left(ValidationError.InvalidDataLength)
        } else {
          create(bytes.slice(1, 1 + KeyLength), dataType = DataType.PublicKeyAccount)
        }

        case Some(DataType.Address) => if (bytes.length < 1 + Address.AddressLength) {
          Left(ValidationError.InvalidDataLength)
        } else {
          create(bytes.slice(1, 1 + Address.AddressLength), dataType = DataType.Address)
        }

        case Some(DataType.Amount) => if (bytes.length < 1 + 8) {
          Left(ValidationError.InvalidDataLength)
        } else {
          create(bytes.slice(1, 1 + 8), dataType = DataType.Amount)
        }

        case _ => Left(ValidationError.InvalidDataType)
      }
    } else {
      Left(ValidationError.InvalidDataLength)
    }
}

  def fromArrayBytes(bytes: Array[Byte]): Either[ValidationError, Seq[DataEntry]] = {

    if (bytes.length > 0) {
      DataType.fromByte(bytes(0)) match {
        case Some(DataType.PublicKeyAccount) => if (bytes.length < 1 + KeyLength) {
          Left(ValidationError.InvalidDataLength)
        } else {
          val newBytes = bytes.slice(1 + KeyLength, bytes.length)
          Right(fromBytes(bytes.slice(0, 1 + KeyLength)).right.get +: fromArrayBytes(newBytes).right.get)
        }

        case Some(DataType.Address) => if (bytes.length < 1 + Address.AddressLength) {
          Left(ValidationError.InvalidDataLength)
        } else {
          val newBytes = bytes.slice(1 + Address.AddressLength, bytes.length)
          Right(fromBytes(bytes.slice(0, 1 + Address.AddressLength)).right.get +: fromArrayBytes(newBytes).right.get)
        }

        case Some(DataType.Amount) => if (bytes.length < 1 + 8) {
          Left(ValidationError.InvalidDataLength)
        } else {
          val newBytes = bytes.slice(1 + 8, bytes.length)
          Right(fromBytes(bytes.slice(0, 1 + 8)).right.get +: fromArrayBytes(newBytes).right.get)
        }

        case _ => Left(ValidationError.InvalidDataType)
      }
    } else {
      Right(Seq[DataEntry]())
    }
  }
}
