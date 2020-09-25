package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.contract.DataType._

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

  implicit def int2Byte(x:Int): Byte = x.toByte

  implicit def dataEntry2Int(x: DataEntry): Int = Int32.deserializer(x.data)

  implicit def dataEntry2BigInt(x: DataEntry): BigInt = BigInteger.deserializer(x.data)

  implicit def boolDataEntry2Byte(x: DataEntry): Byte = x.data(0)

  private implicit def dataEntry2Long(x: DataEntry): Long = Amount.deserializer(x.data)

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
      case DataTypeObj => x.dataType match {
        case Int32 | Amount | Timestamp | BigInteger => {
          val to: DataTypeVal[_] = DataTypeObj.deserializer(t.data)
          to match {
            case Int32      => formatResB[Int]   (x.data, Int32)
            case Amount     => formatResB[Long]  (x.data, Amount)
            case Timestamp  => formatResB[Long]  (x.data, Timestamp)
            case BigInteger => formatResB[BigInt](x.data, BigInteger)
            case _ => Left(ContractUnsupportedOPC)
          }
        }
        case _ => Left(ContractUnsupportedOPC)
      }
      case _ => Left(ContractInvalidOPCData)
    }

  def concat(x: DataEntry, y: DataEntry): Either[ValidationError, DataEntry] =
    DataEntry.create(x.data ++ y.data, DataType.ShortBytes)

  def constantGet(constant: Array[Byte]): Either[ValidationError, DataEntry] = 
    DataEntry.fromBytes(constant)

  object BasicType extends Enumeration {
    sealed case class basicTypeVal(
      basicType: Int,
      len: Int,
      indexes: Seq[Int],
      op: (Array[Byte], Seq[DataEntry]) => Either[ValidationError, DataEntry])
    extends Val(basicType) { def *(n: Int): Int = n * basicType }

    val Add         = basicTypeVal(1,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), add))
    val Minus       = basicTypeVal(2,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), minus))
    val Multiply    = basicTypeVal(3,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), multiply))
    val Divide      = basicTypeVal(4,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), divide))
    val Minimum     = basicTypeVal(5,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), minimum))
    val Maximum     = basicTypeVal(6,  4, Seq(1, 2), (b, d) => numBiOperation(d(b(1)), d(b(2)), maximum))
    val Concat      = basicTypeVal(7,  4, Seq(1, 2), (b, d) => concat(d(b(1)), d(b(2))))
    val ConstantGet = basicTypeVal(8,  2, Seq(),     (b, d) => constantGet(b.slice(1, b.length-1)))
    val SqrtBigInt  = basicTypeVal(9,  3, Seq(1),    (b, d) => sqrt(d(b(1))))
    val Convert     = basicTypeVal(10, 4, Seq(1, 2), (b, d) => convertion(d(b(1)), d(b(2))))
    val And         = basicTypeVal(11, 4, Seq(1, 2), (b, d) => boolBiOperation(d(b(1)), d(b(2)), bAnd))
    val Or          = basicTypeVal(12, 4, Seq(1, 2), (b, d) => boolBiOperation(d(b(1)), d(b(2)), bOr))
    val Xor         = basicTypeVal(13, 4, Seq(1, 2), (b, d) => boolBiOperation(d(b(1)), d(b(2)), bXor))
    val Not         = basicTypeVal(14, 3, Seq(1),    (b, d) => not(d(b(1))))
  }

  // res is call-by-name
  private def updateStack(dataStack: Seq[DataEntry], pointer: Byte,
    res: => Either[ValidationError, DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    if (pointer > dataStack.length || pointer < 0) Left(ContractLocalVariableIndexOutOfRange)
    else res.map(r => dataStack.patch(pointer, Seq(r), 1))

  private def formatRes[T] (res: T, dt: DataTypeVal[T]): Either[ValidationError, DataEntry] =
    formatResB[T](dt.serializer(res), dt)

  private def formatResB[T] (res: Array[Byte], dt: DataTypeVal[T]): Either[ValidationError, DataEntry] =
    DataEntry.create(res, dt).left.map(_ => ValidationError.OverflowError)

  def differ(bytes: Array[Byte], data: Seq[DataEntry], t: BasicType.basicTypeVal) = updateStack(data, bytes.last, t.op(bytes, data))

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(f => Try(BasicType(f)).toOption) match {
      case Some(t: BasicType.basicTypeVal) if checkBytesLength(bytes, t) && checkIndex(bytes, data, t) => differ(bytes, data, t)
      case _ => Left(ContractInvalidOPCData)
    }

  private def checkBytesLength(bytes: Array[Byte], t: BasicType.basicTypeVal): Boolean = {
    (t == BasicType.ConstantGet && bytes.length > t.len) || (bytes.length == t.len)
  }

  private def checkIndex(bytes: Array[Byte], dataStack: Seq[DataEntry], t: BasicType.basicTypeVal): Boolean =
    t.indexes.map(bytes(_)).filterNot(idx =>  idx < dataStack.length && idx >= 0).isEmpty

}
