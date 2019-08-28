package vsys.blockchain.contract

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.crypto.encode.Base58
import vsys.account.{Address, ContractAccount, PublicKeyAccount}
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.contract.RegisterContractTransaction.MaxDescriptionSize
import vsys.blockchain.transaction.TransactionParser.{AmountLength, KeyLength}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.InvalidDataEntry

import scala.util.Success

case class DataEntry(data: Array[Byte],
                     dataType: DataType.Value) {

  lazy val bytes: Array[Byte] = Array(dataType.id.asInstanceOf[Byte]) ++ data

  lazy val json: JsObject = Json.obj(
    "data" -> toJson(data, dataType),
    "type" -> dataType
  )

  private def toJson(d: Array[Byte], t: DataType.Value): JsValue = {
    t match {
      case DataType.PublicKey => Json.toJson(PublicKeyAccount(d).address)
      case DataType.Address => Json.toJson(Address.fromBytes(d).right.get.address)
      case DataType.Amount => Json.toJson(Longs.fromByteArray(d))
      case DataType.Int32 => Json.toJson(Ints.fromByteArray(d))
      case DataType.ShortText => Json.toJson(Base58.encode(d))
      case DataType.ContractAccount => Json.toJson(ContractAccount.fromBytes(d).right.get.address)
      case DataType.TokenId => Json.toJson(ByteStr(d).base58)
    }
  }
}

object DataEntry {

  def create(data: Array[Byte], dataType: DataType.Value): Either[ValidationError, DataEntry] = {
    dataType match {
      case DataType.ShortText if checkDataType(Shorts.toByteArray(data.length.toShort) ++ data, dataType) => Right(DataEntry(Shorts.toByteArray(data.length.toShort) ++ data, dataType))
      case _ if checkDataType(data, dataType) => Right(DataEntry(data, dataType))
      case _ => Left(InvalidDataEntry)
    }
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, DataEntry] = {
    if (bytes.length == 0 || DataType.fromByte(bytes(0)).isEmpty)
      Left(InvalidDataEntry)
    else
      DataType.fromByte(bytes(0)) match {
        case Some(DataType.ShortText) => create(bytes.slice(3, bytes.length), DataType(bytes(0)))
        case _ => create(bytes.tail, DataType(bytes(0)))
      }
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
        Right((DataEntry(bytes.slice(position + 1, position + 1 + KeyLength), DataType.PublicKey), position + 1 + KeyLength))
      case Some(DataType.Address) if checkDataType(bytes.slice(position + 1, position + 1 + Address.AddressLength), DataType.Address) =>
        Right((DataEntry(bytes.slice(position + 1, position + 1 + Address.AddressLength), DataType.Address), position + 1 + Address.AddressLength))
      case Some(DataType.Amount) if checkDataType(bytes.slice(position + 1, position + 1 + AmountLength), DataType.Amount) =>
        Right((DataEntry(bytes.slice(position + 1, position + 1 + AmountLength), DataType.Amount), position + 1 + AmountLength))
      case Some(DataType.Int32) if checkDataType(bytes.slice(position + 1, position + 1 + 4), DataType.Int32) =>
        Right((DataEntry(bytes.slice(position + 1, position + 1 + 4), DataType.Int32), position + 1 + 4))
      case Some(DataType.ShortText) if checkDataType(bytes.slice(position + 1, position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))), DataType.ShortText) =>
        Right((DataEntry(bytes.slice(position + 1, position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))), DataType.ShortText), position + 3 + Shorts.fromByteArray(bytes.slice(position + 1, position + 3))))
      case Some(DataType.ContractAccount) if checkDataType(bytes.slice(position + 1, position + 1 + ContractAccount.AddressLength), DataType.ContractAccount) =>
        Right((DataEntry(bytes.slice(position + 1, position + 1 + ContractAccount.AddressLength), DataType.ContractAccount), position + 1 + ContractAccount.AddressLength))
      case Some(DataType.TokenId) if checkDataType(bytes.slice(position + 1, position + 1 + ContractAccount.TokenAddressLength), DataType.TokenId) =>
        Right((DataEntry(bytes.slice(position + 1, position + 1 + ContractAccount.TokenAddressLength), DataType.TokenId), position + 1 + ContractAccount.TokenAddressLength))
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
