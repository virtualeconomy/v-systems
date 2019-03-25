package vsys.contract

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import play.api.libs.json.{JsObject, Json}
import scorex.account.Address
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.{AmountLength, KeyLength}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidDataEntry
import vsys.account.ContractAccount
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
      case Success(byteArray) => parseArrays(byteArray)
      case _ => Left(InvalidDataEntry)
    }
  }

  def parseArraySize(bytes: Array[Byte], position: Int): Either[ValidationError, (DataEntry, Int)] = {
    DataType.fromByte(bytes(position)) match {
      case Some(DataType.PublicKey) if checkDataType(bytes.slice(position + 1, position + 1 + KeyLength), DataType.PublicKey) =>
        Right((buildDataEntry(bytes.slice(position + 1, position + 1 + KeyLength), DataType.PublicKey), position + 1 + KeyLength))
      case Some(DataType.Address) if checkDataType(bytes.slice(position + 1, position + 1 + Address.AddressLength), DataType.Address) =>
        Right((buildDataEntry(bytes.slice(position + 1, position + 1 + Address.AddressLength), DataType.Address), position + 1 + Address.AddressLength))
      case Some(DataType.Amount) if checkDataType(bytes.slice(position + 1, position + 1 + AmountLength), DataType.Amount) =>
        Right((buildDataEntry(bytes.slice(position + 1, position + 1 + AmountLength), DataType.Amount), position + 1 + AmountLength))
      case Some(DataType.Int32) if checkDataType(bytes.slice(position + 1, position + 1 + 4), DataType.Int32) =>
        Right((buildDataEntry(bytes.slice(position + 1, position + 1 + 4), DataType.Int32), position + 1 + 4))
      case Some(DataType.ShortText) if checkDataType(bytes.slice(position + 1, position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))), DataType.ShortText) =>
        Right((buildDataEntry(bytes.slice(position + 1, position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))), DataType.ShortText), position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))))
      case Some(DataType.ContractAccount) if checkDataType(bytes.slice(position + 1, position + 1 + ContractAccount.AddressLength), DataType.ContractAccount) =>
        Right((buildDataEntry(bytes.slice(position + 1, position + 1 + ContractAccount.AddressLength), DataType.ContractAccount), position + 1 + ContractAccount.AddressLength))
      case _ => Left(InvalidDataEntry)
    }
  }

  def serializeArrays(ds: Seq[DataEntry]): Array[Byte] = {
    Shorts.toByteArray(ds.length.toShort) ++ Bytes.concat(ds.map(_.bytes): _*)
  }

  def right(structure: (Seq[DataEntry], Int)): Either[ValidationError, (Seq[DataEntry], Int)] = Right(structure)

  def parseArrays(bytes: Array[Byte]): Either[ValidationError, Seq[DataEntry]] = {
    val length = Shorts.fromByteArray(bytes.slice(0, 2))
    (0 until length).foldLeft(right((Seq.empty[DataEntry], 2))) {
      case (accPos, _) => accPos.flatMap(ap => parseArraySize(bytes, ap._2) match {
        case Right((arr, nextPos)) => Right((ap._1 :+ arr, nextPos))
        case Left(l) => Left(l)
      })

    } match {
      case Right((acc, _)) => Right(acc)
      case Left(l) => Left(l)
    }
  }

  private def buildDataEntry(data: Array[Byte], dataType: DataType.Value): DataEntry = {
    dataType match {
      case DataType.ShortText => DataEntry(data.slice(2, data.length), dataType)
      case _ => DataEntry(data, dataType)
    }
  }

  private def checkDataType(data: Array[Byte], dataType: DataType.Value): Boolean = dataType match {
      case DataType.PublicKey => data.length == KeyLength
      case DataType.Address => Address.fromBytes(data).isRight
      case DataType.Amount => data.length == AmountLength && Longs.fromByteArray(data) >= 0
      case DataType.Int32 => data.length == 4 && Ints.fromByteArray(data) >= 0
      case DataType.ShortText => Shorts.fromByteArray(data.slice(0, 2)) + 2 == data.length && data.length <= 2 + MaxDescriptionSize
      case DataType.ContractAccount => ContractAccount.fromBytes(data).isRight
      case _ => false
  }

}
