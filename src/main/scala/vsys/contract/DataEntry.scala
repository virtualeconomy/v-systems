package vsys.contract

import com.google.common.primitives.{Ints, Longs, Shorts, Bytes}
import play.api.libs.json.{JsObject, Json}
import scorex.account.Address
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.{AmountLength, KeyLength}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidDataEntry
import vsys.transaction.contract.RegisterContractTransaction.MaxDescriptionSize

import scala.util.Success

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
    if (checkDataType(data, dataType))
      Right(buildDataEntry(data, dataType))
    else
      Left(InvalidDataEntry)
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, DataEntry] = {
    if (bytes.length == 0 || DataType.fromByte(bytes(0)).isEmpty)
      Left(InvalidDataEntry)
    else
      create(bytes.tail, DataType(bytes(0)))
  }


  def fromBase58String(base58String: String): Either[ValidationError, Seq[DataEntry]] = {
    Base58.decode(base58String) match {
      case Success(byteArray) => Right(parseArrays(byteArray))
      case _ => Left(InvalidDataEntry)
    }
  }

  def parseArraySize(bytes: Array[Byte], position: Int): Either[ValidationError, (DataEntry, Int)] = {
    DataType.fromByte(bytes(position)) match {
      case Some(DataType.PublicKey) => Right((create(bytes.slice(position + 1, position + 1 + KeyLength), DataType.PublicKey).right.get, position + 1 + KeyLength))
      case Some(DataType.Address) => Right((create(bytes.slice(position + 1, position + 1 + Address.AddressLength), DataType.Address).right.get, position + 1 + Address.AddressLength))
      case Some(DataType.Amount) => Right((create(bytes.slice(position + 1, position + 1 + AmountLength), DataType.Amount).right.get, position + 1 + AmountLength))
      case Some(DataType.Int32) => Right((create(bytes.slice(position + 1, position + 1 + 4), DataType.Int32).right.get, position + 1 + 4))
      case Some(DataType.ShortText) => Right((create(bytes.slice(position + 1, position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))),
        DataType.ShortText).right.get, position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))))
      case _ => Left(InvalidDataEntry)
    }
  }

  def serializeArrays(ds: Seq[DataEntry]): Array[Byte] = {
    Shorts.toByteArray(ds.length.toShort) ++ Bytes.concat(ds.map(_.bytes): _*)
  }

  def parseArrays(bytes: Array[Byte]): Seq[DataEntry] = {
    val length = Shorts.fromByteArray(bytes.slice(0, 2))
    val r = (0 until length).foldLeft((Seq.empty[DataEntry], 2)) {
      case ((acc, pos), _) =>
        val (arr, nextPos) = parseArraySize(bytes, pos).right.get
        (acc :+ arr, nextPos)
    }
    r._1
  }

  private def buildDataEntry(data: Array[Byte], dataType: DataType.Value): DataEntry = {
    dataType match {
      case DataType.ShortText => DataEntry(data.slice(2, data.length), dataType)
      case _ => DataEntry(data, dataType)
    }
  }

  private def checkDataType(data: Array[Byte], dataType: DataType.Value): Boolean = {
    dataType match {
      case DataType.PublicKey => data.length == KeyLength
      case DataType.Address => Address.fromBytes(data).isRight
      case DataType.Amount => data.length == AmountLength && Longs.fromByteArray(data) >= 0
      case DataType.Int32 => data.length == 4 && Ints.fromByteArray(data) >= 0
      case DataType.ShortText => Shorts.fromByteArray(data.slice(0, 2)) + 2 == data.length && data.length <= 2 + MaxDescriptionSize
      case _ => false
    }
  }

}
