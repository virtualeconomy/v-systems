package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right, Try}

object CompareOpcDiff extends OpcDiffer {

  def geq(context: ExecutionContext)(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
                                     pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (x.dataType == y.dataType) {
      val supportList = List(DataType.Amount, DataType.Timestamp)
      supportList.find(a => a == x.dataType) match {
        case Some(_) => {
          val xValue = Longs.fromByteArray(x.data)
          val yValue = Longs.fromByteArray(y.data)
          if (xValue >= yValue) {
            Right(dataStack.patch(pointer, Seq(DataEntry(Array(1.toByte), DataType.Boolean)), 1))
          } else {
            Right(dataStack.patch(pointer, Seq(DataEntry(Array(0.toByte), DataType.Boolean)), 1))
          }
        }
        case None => Left(ContractUnsupportedOPC)
      }
    } else Left(ContractDataTypeMismatch)
  }

  object CompareType extends Enumeration {
    val Geq = Value(1)
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(f => Try(CompareType(f)).toOption) match {
      case Some(CompareType.Geq) if bytes.length == 3 => geq(context)(data(bytes(0)), data(bytes(1)), data, bytes(2))
      case _ => Left(ContractInvalidOPCData)
    }
}
