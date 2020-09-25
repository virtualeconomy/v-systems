package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Ints
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.Try

trait OpcDiffer {

  protected val defaultTokenIndex = DataEntry(Ints.toByteArray(0), DataType.Int32)

  protected def checkData(bytes: Array[Byte], dataLength: Int, operandCount: Int, withTokenIndex: Boolean = true): Boolean =
    (bytes.length == 1 || (bytes.tail.max < dataLength && bytes.tail.min >= 0)) && (bytes.length == operandCount + 1 || (withTokenIndex && bytes.length == operandCount))

  protected def tokenIndex(bytes: Array[Byte], data: Seq[DataEntry], pos: Int) =
    Try(data(bytes(pos))).toOption.getOrElse(defaultTokenIndex)

  def parseBytesDf(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] =
    Right(OpcDiff.empty)

  def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    Right(data)

  def parseBytes(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] =
    for {
      diff <- parseBytesDf(context)(bytes, data)
      data <- parseBytesDt(context)(bytes, data)
    } yield (diff, data)
}

object OpcDiffer {
  object OpcType extends Enumeration {
    sealed case class OpcTypeVal(opcType: Int,
                                 opcDiffer: OpcDiffer) extends Val(opcType) {
      def *(n: Int): Int = n * opcType
    }
    val SystemOpc  = OpcTypeVal(0, SystemTransferDiff)
    val AssertOpc  = OpcTypeVal(1, AssertOpcDiff)
    val LoadOpc    = OpcTypeVal(2, LoadOpcDiff)
    val CDBVOpc    = OpcTypeVal(3, CDBVOpcDiff)
    val CDBVROpc   = OpcTypeVal(4, CDBVROpcDiff)
    val TDBOpc     = OpcTypeVal(5, TDBOpcDiff)
    val TDBROpc    = OpcTypeVal(6, TDBROpcDiff)
    val TDBAOpc    = OpcTypeVal(7, TDBAOpcDiff)
    val TDBAROpc   = OpcTypeVal(8, TDBAROpcDiff)
    val ReturnOpc  = OpcTypeVal(9, ReturnOpcDiff)
    val CompareOpc = OpcTypeVal(10, CompareOpcDiff)
    val BasicOpc   = OpcTypeVal(11, BasicOpcDiff)

    def fromByte(implicit b: Byte): Option[OpcTypeVal] =
      Try(OpcType(b).asInstanceOf[OpcTypeVal]).toOption
  }

  def apply(context: ExecutionContext)(opc: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] =
    opc.headOption.flatMap(OpcType.fromByte(_)).toRight(ContractUnsupportedOPC).flatMap(_.opcDiffer.parseBytes(context)(opc.tail, data))

  // res is call-by-name
  def updateStack(dataStack: Seq[DataEntry], pointer: Byte, res: => Either[ValidationError, DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    if (pointer > dataStack.length || pointer < 0) Left(ContractLocalVariableIndexOutOfRange)
    else res.map(r => dataStack.patch(pointer, Seq(r), 1))

  // indexes.max < bytes.length should be ensured before calling this
  def checkIndexes(bytes: Array[Byte], dataStack: Seq[DataEntry], indexes: Seq[Int]): Boolean =
    indexes.map(bytes(_)).filterNot(idx =>  idx < dataStack.length && idx >= 0).isEmpty
}
