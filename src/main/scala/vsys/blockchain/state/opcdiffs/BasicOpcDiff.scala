package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.contract.DataType._
import vsys.blockchain.contract.DataEntry.ConvertHelper._
import vsys.blockchain.state.opcdiffs.OpcDiffer._

import scala.language.implicitConversions
import scala.util.{Left, Try}

object BasicOpcDiff extends OpcDiffer {

  case class NumBiOperator(int$1: (Int, Int) => Int, long$1: (Long, Long) => Long, bigInt$1: (BigInt, BigInt) => BigInt) {
    val int: (Int, Int) => Try[Int] = (x, y) => Try(int$1(x, y))
    val long: (Long, Long) => Try[Long] = (x, y) => Try(long$1(x, y))
    val bigInt: (BigInt, BigInt) => Try[BigInt] = (x, y) => Try(bigInt$1(x, y))
  }
  val add      = NumBiOperator(Math.addExact,      Math.addExact,      _ + _)
  val minus    = NumBiOperator(Math.subtractExact, Math.subtractExact, _ - _)
  val multiply = NumBiOperator(Math.multiplyExact, Math.multiplyExact, _ * _)
  val divide   = NumBiOperator(Math.floorDiv,      Math.floorDiv,      _ / _)
  val minimum  = NumBiOperator(Math.min,           Math.min,           (x, y) => x.min(y))
  val maximum  = NumBiOperator(Math.max,           Math.max,           (x, y) => x.max(y))

  // babylonian method
  def sqrtBigInt(y: BigInt): BigInt = {
    if (y > 3) Stream.iterate((y, (y >> 1) + 1)){ case (z, x) => (x, (y / x + x) >> 1) }.dropWhile{ case(z, x) => x < z }.head._1
    else if (y > 0) 1 else 0
  }

  // not converting byte to boolean
  case class BoolByteBiOperator(val op: (Byte, Byte) => Byte)

  val bAnd = BoolByteBiOperator(_ & _)
  val bOr  = BoolByteBiOperator(_ | _)
  val bXor = BoolByteBiOperator(_ ^ _)

  val bNot: Byte => Byte = x => ~x & 1

  private implicit def try2Either(t: Try[Either[ValidationError, DataEntry]]): Either[ValidationError, DataEntry] =
    t.recover({ case _ => Left(OverflowError) }).get

  def numBiOperation(x: DataEntry, y: DataEntry, operator: NumBiOperator): Either[ValidationError, DataEntry] =
    if (x.dataType == y.dataType) x.dataType match {
      case Int32      => operator.int(x, y).map(formatRes[Int](_, Int32))
      case Amount     => operator.long(x, y).map(formatRes[Long](_, Amount))
      case Timestamp  => operator.long(x, y).map(formatRes[Long](_, Timestamp))
      case BigInteger => operator.bigInt(x, y).map(formatRes[BigInt](_, BigInteger))
      case _ => Left(ContractUnsupportedOPC)
    } else Left(ContractDataTypeMismatch)

  def sqrt(x: DataEntry): Either[ValidationError, DataEntry] =
    x.dataType match {
      case BigInteger => {
        val value = BigInteger.deserializer(x.data)
        if (value < 0) Left(ValidationError.OverflowError)
        else formatRes[BigInt](sqrtBigInt(value), BigInteger)
      }
      case _ => Left(ContractUnsupportedOPC)
    }

  def boolBiOperation(x: DataEntry, y: DataEntry, operator: BoolByteBiOperator): Either[ValidationError, DataEntry] =
    if (x.dataType == y.dataType && x.dataType == Boolean) formatResB[Boolean](Array(operator.op(x, y)), Boolean)
    else Left(ContractInvalidOPCData)

  def not(x: DataEntry): Either[ValidationError, DataEntry] =
    x.dataType match {
      case Boolean => formatResB[Boolean](Array(bNot(x)), Boolean)
      case _ => Left(ContractUnsupportedOPC)
    }

  def convertion(x: DataEntry, t: DataEntry): Either[ValidationError, DataEntry] =
    t.dataType match {
      case DataTypeObj =>(x.dataType match {
        case Int32 | Amount | Timestamp => Right(x.data) 
        case BigInteger => Right(x.data.drop(2))
        case _ => Left(ContractUnsupportedOPC)
      }) flatMap { bytes => {
          val to: DataTypeVal[_] = DataTypeObj.deserializer(t.data)
          to match {
            case Int32      => formatResB[Int]   (addLeadingZeros(bytes, 4), Int32)
            case Amount     => formatResB[Long]  (addLeadingZeros(bytes, 8), Amount)
            case Timestamp  => formatResB[Long]  (addLeadingZeros(bytes, 8), Timestamp)
            case BigInteger => formatResB[BigInt](bytes.dropRight(1).dropWhile(i => i == 0) ++ bytes.takeRight(1), BigInteger)
            case _ => Left(ContractUnsupportedOPC)
          }
        }
      }
      case _ => Left(ContractInvalidOPCData)
    }

  def addLeadingZeros(in: Array[Byte], length: Int): Array[Byte] =
    if (in.length - length > 0) {
      in.take(in.length - length).dropWhile(i => i == 0) ++ in.takeRight(length)
    } else {
      (new Array[Byte](0.max(length - in.length)) ++ in).takeRight(length)
    }

  def concat(x: DataEntry, y: DataEntry): Either[ValidationError, DataEntry] =
    DataEntry.create(x.data ++ y.data, DataType.ShortBytes)

  def constantGet(constant: Array[Byte]): Either[ValidationError, DataEntry] =
    DataEntry.fromBytes(constant)

  object BasicType extends Enumeration {
    sealed case class BasicTypeVal(
      basicType: Int,
      len: Int,
      indexes: Seq[Int],
      op: (Array[Byte], Seq[DataEntry]) => Either[ValidationError, DataEntry])
    extends Val(basicType) { def *(n: Int): Int = n * basicType }

    val Add         = BasicTypeVal(1,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), add))
    val Minus       = BasicTypeVal(2,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), minus))
    val Multiply    = BasicTypeVal(3,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), multiply))
    val Divide      = BasicTypeVal(4,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), divide))
    val Minimum     = BasicTypeVal(5,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), minimum))
    val Maximum     = BasicTypeVal(6,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), maximum))
    val Concat      = BasicTypeVal(7,  4, Seq(1, 2), (b, d) => concat(d(b(1)), d(b(2))))
    val ConstantGet = BasicTypeVal(8,  2, Seq(),     (b, d) => constantGet(b.slice(1, b.length-1)))
    val SqrtBigInt  = BasicTypeVal(9,  3, Seq(1),    (b, d) => sqrt(d(b(1))))
    val Convert     = BasicTypeVal(10, 4, Seq(1, 2), (b, d) => convertion(d(b(1)), d(b(2))))
    val And         = BasicTypeVal(11, 4, Seq(1, 2), (b, d) => boolBiOperation(d(b(1)), d(b(2)), bAnd))
    val Or          = BasicTypeVal(12, 4, Seq(1, 2), (b, d) => boolBiOperation(d(b(1)), d(b(2)), bOr))
    val Xor         = BasicTypeVal(13, 4, Seq(1, 2), (b, d) => boolBiOperation(d(b(1)), d(b(2)), bXor))
    val Not         = BasicTypeVal(14, 3, Seq(1),    (b, d) => not(d(b(1))))
  }

  private def formatRes[T] (res: T, dt: DataTypeVal[T]): Either[ValidationError, DataEntry] =
    formatResB[T](dt.serializer(res), dt)

  private def formatResB[T] (res: Array[Byte], dt: DataTypeVal[T]): Either[ValidationError, DataEntry] =
    DataEntry.create(res, dt).left.map(_ => ValidationError.OverflowError)

  def differ(bytes: Array[Byte], data: Seq[DataEntry], t: BasicType.BasicTypeVal) = updateStack(data, bytes.last, t.op(bytes, data))

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(f => Try(BasicType(f)).toOption) match {
      case Some(t: BasicType.BasicTypeVal) if checkBytesLength(bytes, t) && checkIndexes(bytes, data, t.indexes) => differ(bytes, data, t)
      case _ => Left(ContractInvalidOPCData)
    }

  private def checkBytesLength(bytes: Array[Byte], t: BasicType.BasicTypeVal): Boolean = {
    (t == BasicType.ConstantGet && bytes.length > t.len) || (bytes.length == t.len)
  }
}
