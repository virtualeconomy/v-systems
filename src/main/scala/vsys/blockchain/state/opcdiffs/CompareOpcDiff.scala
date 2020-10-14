package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}
import vsys.blockchain.contract.DataType._
import vsys.blockchain.contract.DataEntry.ConvertHelper._
import vsys.blockchain.state.opcdiffs.OpcDiffer._

import scala.language.implicitConversions
import scala.util.{Left, Right, Try}

object CompareOpcDiff extends OpcDiffer {

  case class NumComparator(int: (Int, Int) => Boolean, long: (Long, Long) => Boolean, bigInt: (BigInt, BigInt) => Boolean)

  val ge  = NumComparator(_ >= _, _ >= _, _ >= _)
  val gt  = NumComparator(_ >  _, _ >  _, _ >  _)
  val le  = NumComparator(_ <= _, _ <= _, _ <= _)
  val lt  = NumComparator(_ <  _, _ <  _, _ <  _)
  val _eq = NumComparator(_ == _, _ == _, _ == _)
  val _ne = NumComparator(_ != _, _ != _, _ != _)

  private implicit def booleanToDataEntry(b: Boolean): DataEntry = DataEntry(Array((if(b) 1 else 0).toByte), DataType.Boolean)

  def numBiComparation(x: DataEntry, y: DataEntry, comparator: NumComparator): Either[ValidationError, DataEntry] =
    if (x.dataType == y.dataType) x.dataType match {
      case Int32      => Right(comparator.int(x, y))
      case Amount     => Right(comparator.long(x, y))
      case Timestamp  => Right(comparator.long(x, y))
      case BigInteger => Right(comparator.bigInt(x, y))
      case _ => Left(ContractUnsupportedOPC)
    } else Left(ContractDataTypeMismatch)

  object CompareType extends Enumeration {
    class CompareTypeVal(compareType: Int) extends Val(compareType) { def *(n: Int): Int = n * compareType }
    sealed case class NumCompareTypeVal(compareType: Int, op: NumComparator) extends CompareTypeVal(compareType)
    sealed case class BytesCompareTypeVal(compareType: Int, op: (DataEntry, DataEntry) => Boolean) extends CompareTypeVal(compareType)
    val Ge  = NumCompareTypeVal(1, ge)
    val Gt  = NumCompareTypeVal(2, gt)
    val Le  = NumCompareTypeVal(3, le)
    val Lt  = NumCompareTypeVal(4, lt)
    val Eq  = NumCompareTypeVal(5, _eq)
    val Ne  = NumCompareTypeVal(6, _ne)
    val Beq = BytesCompareTypeVal(7, _ == _)
    val Bne = BytesCompareTypeVal(8, _ != _)
  }

  def differ(bytes: Array[Byte], data: Seq[DataEntry], t: CompareType.NumCompareTypeVal) =
    updateStack(data, bytes.last, numBiComparation(data(bytes(1)), data(bytes(2)), t.op))

  def differ(bytes: Array[Byte], data: Seq[DataEntry], t: CompareType.BytesCompareTypeVal) =
    updateStack(data, bytes.last, Right(t.op(data(bytes(1)), data(bytes(2)))))

  private val ensured = (b: Array[Byte], d: Seq[DataEntry]) => b.length == 4 && checkIndexes(b, d, Seq(1, 2))
  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(f => Try(CompareType(f)).toOption) match {
      case Some(t: CompareType.NumCompareTypeVal) if ensured(bytes, data) => differ(bytes, data, t)
      case Some(t: CompareType.BytesCompareTypeVal) if ensured(bytes, data) => differ(bytes, data, t)
      case _ => Left(ContractInvalidOPCData)
    }
}
