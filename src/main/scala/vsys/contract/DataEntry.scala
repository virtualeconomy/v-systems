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
    dataType match {
      case DataType.PublicKeyAccount if data.length == KeyLength => Right(DataEntry(data, dataType))
      case DataType.Address if Address.fromBytes(data).isRight => Right(DataEntry(data, dataType))
      case DataType.Amount if data.length == AmountLength && Longs.fromByteArray(data) >= 0 => Right(DataEntry(data, dataType))
      case DataType.Index if data.length == 4 && Ints.fromByteArray(data) >= 0 => Right(DataEntry(data, dataType))
      case DataType.Description if Shorts.fromByteArray(data.slice(0, 2)) + 2 == data.length
        && data.length <= 2 + MaxDescriptionSize => Right(DataEntry(data.slice(2, data.length), dataType))
      case _ => Left(InvalidDataEntry)
    }

  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, DataEntry] = {

    DataType.fromByte(bytes(0)) match {
      case Some(DataType.PublicKeyAccount) => create(bytes.tail, DataType.PublicKeyAccount)
      case Some(DataType.Address) => create(bytes.tail, DataType.Address)
      case Some(DataType.Amount) => create(bytes.tail, DataType.Amount)
      case Some(DataType.Index) => create(bytes.tail, DataType.Index)
      case Some(DataType.Description) => create(bytes.tail, DataType.Description)
      case _ => Left(InvalidDataEntry)
    }
  }


  def fromBase58String(base58String: String): Either[ValidationError, Seq[DataEntry]] = {
    Base58.decode(base58String) match {
      case Success(byteArray) => Right(parseArrays(byteArray))
      case _ => Left(InvalidDataEntry)
    }
  }

  def parseArraySize(bytes: Array[Byte], position: Int): Either[ValidationError, (DataEntry, Int)] = {
    DataType.fromByte(bytes(position)) match {
      case Some(DataType.PublicKeyAccount) => Right((create(bytes.slice(position + 1, position + 1 + KeyLength), DataType.PublicKeyAccount).right.get, position + 1 + KeyLength))
      case Some(DataType.Address) => Right((create(bytes.slice(position + 1, position + 1 + Address.AddressLength), DataType.Address).right.get, position + 1 + Address.AddressLength))
      case Some(DataType.Amount) => Right((create(bytes.slice(position + 1, position + 1 + AmountLength), DataType.Amount).right.get, position + 1 + AmountLength))
      case Some(DataType.Index) => Right((create(bytes.slice(position + 1, position + 1 + 4), DataType.Index).right.get, position + 1 + 4))
      case Some(DataType.Description) => Right((create(bytes.slice(position + 1, position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))),
        DataType.Description).right.get, position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))))
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

}
