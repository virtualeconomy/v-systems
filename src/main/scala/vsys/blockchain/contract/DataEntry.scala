package vsys.blockchain.contract

import com.google.common.primitives.{Shorts, Bytes, Longs, Ints}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.InvalidDataEntry
import vsys.blockchain.contract.DataType._

import scala.language.implicitConversions
import scala.util.Success

case class DataEntry(data: Array[Byte],
                     dataType: DataType.DataTypeVal[_]) {

  lazy val bytes: Array[Byte] = Array(dataType.id.asInstanceOf[Byte]) ++ data

  lazy val json: JsObject = Json.obj(
    "data" -> dataType.jsonifierB(data),
    "type" -> dataType
  )

  override def equals(obj: Any): Boolean = obj match {
    case a: DataEntry => bytes sameElements a.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

}

object DataEntry {

  def create(data: Array[Byte], dataType: DataType.DataTypeVal[_]): Either[ValidationError, DataEntry] =
    if (dataType.lenFixed || dataType == DataType.Account) doCreate(data, dataType) 
    else doCreate(DataType.arrayShortLengthToByteArray(data) ++ data, dataType)

  // length unfixed data array should start with 2 bytes which indicates length of content of data
  private def doCreate(data: Array[Byte], dataType: DataType.DataTypeVal[_]): Either[ValidationError, DataEntry] =
    if (dataType.validator(data)) Right(DataEntry(data, dataType)) else Left(InvalidDataEntry)

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, DataEntry] = {

    if (bytes.length == 0) Left(InvalidDataEntry) else DataType.fromByte(bytes(0)) match {
        case Some(dt) => doCreate(bytes.tail, dt)
        case _ => Left(InvalidDataEntry)
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
          case Some(dt: DataType.DataTypeVal[_]) if (dt.lenFixed && dt.validator(bytes.drop(1).take(dt.maxLen))) =>
            Right((DataEntry(bytes.drop(1).take(dt.maxLen), dt), bytes.drop(1 + dt.maxLen)))
          case Some(dt: DataType.DataTypeVal[_]) if (!dt.lenFixed && dt.maxLen <= Short.MaxValue && bytes.length >= 3
            && dt.validator(bytes.drop(1).take(2 + Shorts.fromByteArray(bytes.drop(1).take(2))))) =>
            Right((DataEntry(bytes.drop(1).take(2 + Shorts.fromByteArray(bytes.drop(1).take(2))), dt),
              bytes.drop(3 + Shorts.fromByteArray(bytes.drop(1).take(2)))))
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

  object ConvertHelper {

    implicit def int2Byte(x:Int): Byte = x.toByte

    implicit def dataEntry2Int(x: DataEntry): Int = Ints.fromByteArray(x.data)

    implicit def dataEntry2BigInt(x: DataEntry): BigInt = BigInteger.deserializer(x.data)

    implicit def dataEntry2Long(x: DataEntry): Long = Longs.fromByteArray(x.data)

    implicit def boolDataEntry2Byte(x: DataEntry): Byte = x.data(0)
  }
}
