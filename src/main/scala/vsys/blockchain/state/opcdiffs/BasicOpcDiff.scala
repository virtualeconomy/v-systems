package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Try}

object BasicOpcDiff extends OpcDiffer {

  def add(context: ExecutionContext)(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
                                     pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (x.dataType == y.dataType) {
      val supportList = List(DataType.Amount, DataType.Timestamp, DataType.Int32)
      supportList.find(a => a == x.dataType) match {
        case Some(_) => {
          if (x.dataType == DataType.Int32) {
            val xValue = Ints.fromByteArray(x.data)
            val yValue = Ints.fromByteArray(y.data)
            if (Try(Math.addExact(xValue, yValue)).isFailure) Left(ValidationError.OverflowError)
            else {
              for {
                zDataEntry <- DataEntry.create(Ints.toByteArray(xValue + yValue), x.dataType)
              } yield dataStack.patch(pointer, Seq(zDataEntry), 1)
            }
          } else {
            val xValue = Longs.fromByteArray(x.data)
            val yValue = Longs.fromByteArray(y.data)
            if (Try(Math.addExact(xValue, yValue)).isFailure) Left(ValidationError.OverflowError)
            else {
              for {
                zDataEntry <- DataEntry.create(Longs.toByteArray(xValue + yValue), x.dataType)
              } yield dataStack.patch(pointer, Seq(zDataEntry), 1)
            }
          }
        }
        case None => Left(ContractUnsupportedOPC)
      }
    } else Left(ContractDataTypeMismatch)
  }

  def minus(context: ExecutionContext)(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
                                       pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (x.dataType == y.dataType) {
      val supportList = List(DataType.Amount, DataType.Timestamp, DataType.Int32)
      supportList.find(a => a == x.dataType) match {
        case Some(_) => {
          if (x.dataType == DataType.Int32) {
            val xValue = Ints.fromByteArray(x.data)
            val yValue = Ints.fromByteArray(y.data)
            if (Try(Math.subtractExact(xValue, yValue)).isFailure) Left(ValidationError.OverflowError)
            else {
              for {
                zDataEntry <- DataEntry.create(Ints.toByteArray(xValue - yValue), x.dataType)
              } yield dataStack.patch(pointer, Seq(zDataEntry), 1)
            }
          } else {
            val xValue = Longs.fromByteArray(x.data)
            val yValue = Longs.fromByteArray(y.data)
            if (Try(Math.subtractExact(xValue, yValue)).isFailure) Left(ValidationError.OverflowError)
            else {
              for {
                zDataEntry <- DataEntry.create(Longs.toByteArray(xValue - yValue), x.dataType)
              } yield dataStack.patch(pointer, Seq(zDataEntry), 1)
            }
          }
        }
        case None => Left(ContractUnsupportedOPC)
      }
    } else Left(ContractDataTypeMismatch)
  }

  def multiply(context: ExecutionContext)(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
                                          pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (x.dataType == y.dataType) {
      val supportList = List(DataType.Amount, DataType.Timestamp, DataType.Int32)
      supportList.find(a => a == x.dataType) match {
        case Some(_) => {
          if (x.dataType == DataType.Int32) {
            val xValue = Ints.fromByteArray(x.data)
            val yValue = Ints.fromByteArray(y.data)
            if (Try(Math.multiplyExact(xValue, yValue)).isFailure) Left(ValidationError.OverflowError)
            else {
              for {
                zDataEntry <- DataEntry.create(Ints.toByteArray(xValue * yValue), x.dataType)
              } yield dataStack.patch(pointer, Seq(zDataEntry), 1)
            }
          } else {
            val xValue = Longs.fromByteArray(x.data)
            val yValue = Longs.fromByteArray(y.data)
            if (Try(Math.multiplyExact(xValue, yValue)).isFailure) Left(ValidationError.OverflowError)
            else {
              for {
                zDataEntry <- DataEntry.create(Longs.toByteArray(xValue * yValue), x.dataType)
              } yield dataStack.patch(pointer, Seq(zDataEntry), 1)
            }
          }
        }
        case None => Left(ContractUnsupportedOPC)
      }
    } else Left(ContractDataTypeMismatch)
  }

  object BasicType extends Enumeration {
    val Add = Value(1)
  }

  override def parseBytesDt(context: ExecutionContext)(bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] =
    bytes.headOption.flatMap(f => Try(BasicType(f)).toOption) match {
      case Some(BasicType.Add) if bytes.length == 4 => add(context)(data(bytes(1)), data(bytes(2)), data, bytes(3))
      case _ => Left(ContractInvalidOPCData)
    }
}
