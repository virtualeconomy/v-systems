package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right, Try}

object BasicOpcDiff extends OpcDiffer {

  def add(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
          pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
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
  }


  def minus(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
            pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
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
  }

  def multiply(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
               pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
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
  }

  def divide(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
             pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      if (x.dataType == y.dataType) {
        val supportList = List(DataType.Amount, DataType.Timestamp, DataType.Int32)
        supportList.find(a => a == x.dataType) match {
          case Some(_) => {
            if (x.dataType == DataType.Int32) {
              val xValue = Ints.fromByteArray(x.data)
              val yValue = Ints.fromByteArray(y.data)
              if (yValue == 0) Left(ValidationError.OverflowError)
              else {
                for {
                  zDataEntry <- DataEntry.create(Ints.toByteArray(xValue / yValue), x.dataType)
                } yield dataStack.patch(pointer, Seq(zDataEntry), 1)
              }
            } else {
              val xValue = Longs.fromByteArray(x.data)
              val yValue = Longs.fromByteArray(y.data)
              if (yValue == 0) Left(ValidationError.OverflowError)
              else {
                for {
                  zDataEntry <- DataEntry.create(Longs.toByteArray(xValue / yValue), x.dataType)
                } yield dataStack.patch(pointer, Seq(zDataEntry), 1)
              }
            }
          }
          case None => Left(ContractUnsupportedOPC)
        }
      } else Left(ContractDataTypeMismatch)
    }
  }

  def minimum(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
                                         pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      if (x.dataType == y.dataType) {
        val supportList = List(DataType.Amount, DataType.Timestamp, DataType.Int32)
        supportList.find(a => a == x.dataType) match {
          case Some(_) => {
            val xValue = if (x.dataType == DataType.Int32) Ints.fromByteArray(x.data) else Longs.fromByteArray(x.data)
            val yValue = if (y.dataType == DataType.Int32) Ints.fromByteArray(y.data) else Longs.fromByteArray(y.data)
            if (xValue > yValue) Right(dataStack.patch(pointer, Seq(y), 1))
            else Right(dataStack.patch(pointer, Seq(x), 1))
          }
          case None => Left(ContractUnsupportedOPC)
        }
      } else Left(ContractDataTypeMismatch)
    }
  }

  def maximum(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
              pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      if (x.dataType == y.dataType) {
        val supportList = List(DataType.Amount, DataType.Timestamp, DataType.Int32)
        supportList.find(a => a == x.dataType) match {
          case Some(_) => {
            val xValue = if (x.dataType == DataType.Int32) Ints.fromByteArray(x.data) else Longs.fromByteArray(x.data)
            val yValue = if (y.dataType == DataType.Int32) Ints.fromByteArray(y.data) else Longs.fromByteArray(y.data)
            if (xValue > yValue) Right(dataStack.patch(pointer, Seq(x), 1))
            else Right(dataStack.patch(pointer, Seq(y), 1))
          }
          case None => Left(ContractUnsupportedOPC)
        }
      } else Left(ContractDataTypeMismatch)
    }
  }

  def concat(x: DataEntry, y: DataEntry, dataStack: Seq[DataEntry],
             pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
    if (pointer > dataStack.length || pointer < 0) {
      Left(ContractLocalVariableIndexOutOfRange)
    } else {
      for {
        res <- DataEntry.create(x.data ++ y.data, DataType.ShortBytes)
      } yield dataStack.patch(pointer, Seq(res), 1)
    }
  }

  def constantGet(constant: Array[Byte], dataStack: Seq[DataEntry],
                  pointer: Byte): Either[ValidationError, Seq[DataEntry]] = {
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

    val Add         = basicTypeVal(1, 4, (b, d) => add(d(b(1)), d(b(2)), d, b(3)))
    val Minus       = basicTypeVal(2, 4, (b, d) => minus(d(b(1)), d(b(2)), d, b(3)))
    val Multiply    = basicTypeVal(3, 4, (b, d) => multiply(d(b(1)), d(b(2)), d, b(3)))
    val Divide      = basicTypeVal(4, 4, (b, d) => divide(d(b(1)), d(b(2)), d, b(3)))
    val Minimum     = basicTypeVal(5, 4, (b, d) => minimum(d(b(1)), d(b(2)), d, b(3)))
    val Maximum     = basicTypeVal(6, 4, (b, d) => maximum(d(b(1)), d(b(2)), d, b(3)))
    val Concat      = basicTypeVal(7, 4, (b, d) => concat(d(b(1)), d(b(2)), d, b(3)))
    val ConstantGet = basicTypeVal(8, 2, (b, d) => constantGet(b.slice(1, b.length-1), d, b(b.length-1)))
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
