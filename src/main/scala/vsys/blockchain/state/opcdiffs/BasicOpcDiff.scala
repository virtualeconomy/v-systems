package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.contract.DataType._

import scala.util.{Left, Right, Try, Success, Failure}

object BasicOpcDiff extends OpcDiffer {

  case class BiOperator(int: (Int, Int) => Try[Int], long: (Long, Long) => Try[Long], bigInt: (BigInt, BigInt) => Try[BigInt])
  val add      = BiOperator((x, y) => Try(Math.addExact(x, y)),      (x, y) => Try(Math.addExact(x, y)),      (x, y) => Success(x + y))
  val minus    = BiOperator((x, y) => Try(Math.subtractExact(x, y)), (x, y) => Try(Math.subtractExact(x, y)), (x, y) => Success(x - y))
  val multiply = BiOperator((x, y) => Try(Math.multiplyExact(x, y)), (x, y) => Try(Math.multiplyExact(x, y)), (x, y) => Success(x * y))
  val divide   = BiOperator((x, y) => Try(Math.floorDiv(x, y)),      (x, y) => Try(Math.floorDiv(x, y)),      (x, y) => Try(x / y))
  val minimum  = BiOperator((x, y) => Try(Math.min(x, y)),           (x, y) => Try(Math.min(x, y)),           (x, y) => Success(x.min(y)))
  val maximum  = BiOperator((x, y) => Try(Math.max(x, y)),           (x, y) => Try(Math.max(x, y)),           (x, y) => Success(x.max(y)))

  private def appendRes[T] (p: Byte, res: T, dt: DataType.DataTypeVal[T], ds: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = for {
    _ <- if(dt.validator(dt.serializer(res))) Right(()) else Left(ValidationError.OverflowError) 
    zDataEntry <- DataEntry.doCreate(dt.serializer(res), dt)
  } yield ds.patch(p, Seq(zDataEntry), 1)

  def operation(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
          pointer: Byte, operator: BiOperator): Either[ValidationError, Seq[DataEntry]] =
    if (pointer > dataStack.length || pointer < 0) Left(ContractLocalVariableIndexOutOfRange)
    else if (x.dataType == y.dataType) x.dataType match {
      case Int32 => operator.int(Int32.deserializer(x.data), Int32.deserializer(y.data)) match {
          case Failure(_) => Left(ValidationError.OverflowError)
          case Success(res: Int) => appendRes[Int](pointer, res,  Int32, dataStack)
        }
      case Amount => operator.long(Amount.deserializer(x.data), Amount.deserializer(y.data)) match {
          case Failure(_) => Left(ValidationError.OverflowError)
          case Success(res: Long) => appendRes[Long](pointer, res,  Amount, dataStack)
        }
      case Timestamp => operator.long(Timestamp.deserializer(x.data), Timestamp.deserializer(y.data)) match {
          case Failure(_) => Left(ValidationError.OverflowError)
          case Success(res: Long) => appendRes[Long](pointer, res,  Timestamp, dataStack)
        }
      case BigInteger => operator.bigInt(BigInteger.deserializer(x.data), BigInteger.deserializer(y.data)) match {
          case Failure(_) => Left(ValidationError.OverflowError)
          case Success(res: BigInt) => appendRes[BigInt](pointer, res,  BigInteger, dataStack)
        }
      case _ => Left(ContractUnsupportedOPC)
    } else Left(ContractDataTypeMismatch)

  def sqrt(x: DataEntry, dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] =
    if (pointer > dataStack.length || pointer < 0) Left(ContractLocalVariableIndexOutOfRange)
    else x.dataType match {
      case BigInteger => {
        val value = BigInteger.deserializer(x.data)
        if (value < 0) Left(ValidationError.OverflowError)
        else appendRes[BigInt](pointer, sqrtBigInt(value),  BigInteger, dataStack)
      }
      case _ => Left(ContractUnsupportedOPC)
    }

  // babylonian method
  def sqrtBigInt(y: BigInt): BigInt = {
    if (y > 3) Stream.iterate((y, (y >> 1) + 1)){ case (z, x) => (x, (y / x + x) >> 1) }.dropWhile{ case(z, x) => x < z }.head._1
    else if (y > 0) 1 else 0
  }
  def concat(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      for {
        res <- DataEntry.create(x.data ++ y.data, DataType.ShortBytes)
      } yield dataStack.patch(pointer, Seq(res), 1)
    }
  }

  def constantGet(constant: Array[Byte], dataStack: Seq[DataEntry], pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      DataEntry.fromBytes(constant) match {
        case Right(v) => Right(dataStack.patch(pointer, Seq(v), 1))
        case Left(e) => Left(e)
      }
    }
  }

  object BasicType extends Enumeration {
    sealed case class basicTypeVal(
      basicType: Int,
      len: Int,
      differ: (Array[Byte], Seq[DataEntry]) => Either[ValidationError, Seq[DataEntry]])
    extends Val(basicType) { def *(n: Int): Int = n * basicType }

    val Add         = basicTypeVal(1, 4, (b, d) => operation(d(b(1)), d(b(2)), d, b(3), add))
    val Minus       = basicTypeVal(2, 4, (b, d) => operation(d(b(1)), d(b(2)), d, b(3), minus))
    val Multiply    = basicTypeVal(3, 4, (b, d) => operation(d(b(1)), d(b(2)), d, b(3), multiply))
    val Divide      = basicTypeVal(4, 4, (b, d) => operation(d(b(1)), d(b(2)), d, b(3), divide))
    val Minimum     = basicTypeVal(5, 4, (b, d) => operation(d(b(1)), d(b(2)), d, b(3), minimum))
    val Maximum     = basicTypeVal(6, 4, (b, d) => operation(d(b(1)), d(b(2)), d, b(3), maximum))
    val Concat      = basicTypeVal(7, 4, (b, d) => concat(d(b(1)), d(b(2)), d, b(3)))
    val ConstantGet = basicTypeVal(8, 2, (b, d) => constantGet(b.slice(1, b.length-1), d, b(b.length-1)))
    val SqrtBigInt  = basicTypeVal(9, 3, (b, d) => sqrt(d(b(1)), d, b(2)))
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(f => Try(BasicType(f)).toOption) match {
      case Some(t: BasicType.basicTypeVal) if checkBytesLength(bytes, t) => t.differ(bytes, data)
      case _ => Left(ContractInvalidOPCData)
    }

  private def checkBytesLength(bytes: Array[Byte], t: BasicType.basicTypeVal): Boolean = {
    (t.basicType == 8 && bytes.length > t.len) || (bytes.length == t.len)
  }
}
