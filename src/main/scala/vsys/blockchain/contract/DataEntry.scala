package vsys.blockchain.contract

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.crypto.encode.Base58
import vsys.account.{Address, AddressScheme, ContractAccount, PublicKeyAccount}
import vsys.account.ContractAccount.ChecksumLength
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.InvalidDataEntry
import vsys.utils.crypto.hash.SecureCryptographicHash._

import scala.util.Success

case class DataEntry(data: Array[Byte],
                     dataType: DataType.DataTypeVal) {

  lazy val bytes: Array[Byte] = Array(dataType.id.asInstanceOf[Byte]) ++ data

  lazy val json: JsObject = Json.obj(
    "data" -> toJson(data, dataType),
    "type" -> dataType
  )

  private def toJson(d: Array[Byte], t: DataType.DataTypeVal): JsValue = {
    t match {
      case DataType.PublicKey => Json.toJson(PublicKeyAccount(d).address)
      case DataType.Address => Json.toJson(Address.fromBytes(d).right.get.address)
      case DataType.Amount => Json.toJson(Longs.fromByteArray(d))
      case DataType.Int32 => Json.toJson(Ints.fromByteArray(d))
      case DataType.ShortText => Json.toJson(Base58.encode(d))
      case DataType.ContractAccount => Json.toJson(ContractAccount.fromBytes(d).right.get.address)
      case DataType.TokenId => Json.toJson(ByteStr(d).base58)
      case DataType.Timestamp => Json.toJson(Longs.fromByteArray(d))
      case DataType.Boolean => Json.toJson(if (d(0) == 1.toByte) "True" else "False")
      case DataType.ShortBytes => Json.toJson(Base58.encode(d))
      case DataType.OpcBlock => Json.toJson("Internal code block")
      case _ => Json.toJson("Unsupported data type")
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case a: DataEntry => bytes sameElements a.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

}

object DataEntry {

  private lazy val scheme = AddressScheme.current.value

  def create(data: Array[Byte], dataType: DataType.DataTypeVal): Either[ValidationError, DataEntry] = {
    dataType match {
      case DataType.ShortText if checkDataType(Shorts.toByteArray(data.length.toShort) ++ data, dataType) => Right(DataEntry(Shorts.toByteArray(data.length.toShort) ++ data, dataType))
      case DataType.ShortBytes if checkDataType(Shorts.toByteArray(data.length.toShort) ++ data, dataType) => Right(DataEntry(Shorts.toByteArray(data.length.toShort) ++ data, dataType))
      case _ if checkDataType(data, dataType) => Right(DataEntry(data, dataType))
      case _ => Left(InvalidDataEntry)
    }
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, DataEntry] = {
    if (bytes.length == 0 || DataType.fromByte(bytes(0)).isEmpty)
      Left(InvalidDataEntry)
    else
      DataType.fromByte(bytes(0)) match {
        case Some(DataType.ShortText) => create(bytes.drop(3), DataType.ShortText)
        case Some(DataType.ShortBytes) => create(bytes.drop(3), DataType.ShortBytes)
        case _ => create(bytes.tail, DataType(bytes(0)).asInstanceOf[DataType.DataTypeVal])
      }
  }

  def fromBase58String(base58String: String): Either[ValidationError, Seq[DataEntry]] = {
    Base58.decode(base58String) match {
      case Success(byteArray) => parseArrays(byteArray)
      case _ => Left(InvalidDataEntry)
    }
  }

  private def parseArray(bytes: Array[Byte]): Either[ValidationError, (DataEntry, Array[Byte])] = {
    bytes.headOption match {
      case Some(b) =>
        DataType.fromByte(b) match {
          case Some(dt: DataType.DataTypeVal) if (dt.lenFixed && checkDataType(bytes.drop(1).take(dt.maxLen), dt)) =>
            Right((DataEntry(bytes.drop(1).take(dt.maxLen), dt), bytes.drop(1 + dt.maxLen)))
          case Some(dt: DataType.DataTypeVal) if (!dt.lenFixed && dt.maxLen <= Short.MaxValue && checkDataType(bytes.drop(1).take(2 + Shorts.fromByteArray(bytes.drop(1).take(2))), dt)) =>
            Right((DataEntry(bytes.drop(1).take(2 + Shorts.fromByteArray(bytes.drop(1).take(2))), dt), bytes.drop(3 + Shorts.fromByteArray(bytes.drop(1).take(2)))))
          case _ => Left(InvalidDataEntry)
        }
      case _ =>
        Left(InvalidDataEntry)
    }
    
  }

  def serializeArrays(ds: Seq[DataEntry]): Array[Byte] = {
    Shorts.toByteArray(ds.length.toShort) ++ Bytes.concat(ds.map(_.bytes): _*)
  }

  def parseArrays(bytes: Array[Byte]): Either[ValidationError, Seq[DataEntry]] = {
    val length = Shorts.fromByteArray(bytes.take(2))
    (0 until length).foldLeft(Right((Seq.empty[DataEntry], bytes.drop(2))): Either[ValidationError, (Seq[DataEntry], Array[Byte])]) {
      case (accPos, _) => accPos.flatMap(ap => parseArray(ap._2) match {
        case Right((arr, suffix)) => Right((ap._1 :+ arr, suffix))
        case Left(l) => Left(l)
      })
    } match {
      case Right((acc, _)) => Right(acc)
      case Left(l) => Left(l)
    }
  }

  private def checkDataType(data: Array[Byte], dataType: DataType.DataTypeVal): Boolean =
    ((dataType.lenFixed && data.length == dataType.maxLen)
      || (!dataType.lenFixed && Shorts.fromByteArray(data.take(2)) == data.drop(2).length && data.drop(2).length <= dataType.maxLen)) &&
    (dataType match {
      case DataType.Address => Address.fromBytes(data).isRight
      case DataType.Amount => Longs.fromByteArray(data) >= 0
      case DataType.Int32 => Ints.fromByteArray(data) >= 0
      case DataType.ContractAccount => ContractAccount.fromBytes(data).isRight
      case DataType.TokenId => isTokenIdValid(data)
      case DataType.Boolean => (data(0) == 1.toByte || data(0) == 0.toByte)
      case DataType.PublicKey | DataType.ShortText | DataType.Timestamp | DataType.ShortBytes | DataType.OpcBlock  => true
      case _ => false
    })

  private def isTokenIdValid(addressBytes: Array[Byte]): Boolean = {
    val version = addressBytes.head
    val network = addressBytes.tail.head
    if (version != ContractAccount.TokenAddressVersion) {
      false
    } else if (network != scheme.chainId) {
      false
    } else {
      if (addressBytes.length != ContractAccount.TokenAddressLength)
        false
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)
        val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))
        checkSum.sameElements(checkSumGenerated)
      }
    }
  }

  private def calcCheckSum(withoutChecksum: Array[Byte]): Array[Byte] = hash(withoutChecksum).take(ChecksumLength)

}
