package vsys.state.opcdiffs

import com.google.common.primitives.Longs
import scorex.account.Address
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.{ContractInvalidInputDataType, ContractInvalidOPCode, GenericError}
import vsys.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right}

object AssertOpcDiff {

  def gtEq0(v: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v.dataType == DataType.Amount && Longs.fromByteArray(v.data) >= 0)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (gteq0): Value ${Longs.fromByteArray(v.data)} is negative"))
  }

  def ltEq(v1: DataEntry, v2: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v1.dataType == DataType.Amount && v2.dataType == DataType.Amount
      && Longs.fromByteArray(v1.data) <= Longs.fromByteArray(v2.data))
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (lteq0): Value ${Longs.fromByteArray(v2.data)} is larger than $v1"))
  }

  def ltInt64(m: DataEntry): Either[ValidationError, OpcDiff] = {
    if (m.dataType == DataType.Amount && Longs.fromByteArray(m.data) <= Long.MaxValue)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (ltint64): Value ${Longs.fromByteArray(m.data)} is invalid"))
  }

  def gt0(v: DataEntry): Either[ValidationError, OpcDiff] = {
    if (v.dataType == DataType.Amount && Longs.fromByteArray(v.data) > 0)
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (gt0): Value $v is non-positive"))
  }

  def eq(add1: DataEntry, add2: DataEntry): Either[ValidationError, OpcDiff] = {
    if (add1.dataType == DataType.Address && add2.dataType == DataType.Address
      && Address.fromBytes(add1.data) == Address.fromBytes(add2.data))
      Right(OpcDiff.empty)
    else if (add1.dataType == DataType.Amount && add2.dataType == DataType.Amount
      && Longs.fromByteArray(add1.data) == Longs.fromByteArray(add2.data))
      Right(OpcDiff.empty)
    else
      Left(GenericError(s"Invalid Assert (eq): DataEntry ${add1.data} is not equal to ${add2.data}"))
  }

  def isCallerOrigin(context: ExecutionContext)(address: DataEntry): Either[ValidationError, OpcDiff] = {
    val signer = context.signers.head
    if (address.dataType != DataType.Address)
      Left(ContractInvalidInputDataType)
    else if (!(address.data sameElements signer.bytes.arr))
      Left(GenericError(s"Address ${address.data} does not equal $signer 's address"))
    else
      Right(OpcDiff.empty)
  }

  def isSignerOrigin(context: ExecutionContext)(address: DataEntry): Either[ValidationError, OpcDiff] = {
    val signer = context.signers.head
    if (address.dataType != DataType.Address)
      Left(ContractInvalidInputDataType)
    else if (!(address.data sameElements signer.bytes.arr))
      Left(GenericError(s"Address ${address.data} does not equal $signer 's address"))
    else
      Right(OpcDiff.empty)
  }

  object AssertType extends Enumeration {
    val GteqZeroAssert = Value(1)
    val LteqAssert = Value(2)
    val LtInt64Assert = Value(3)
    val GtZeroAssert = Value(4)
    val EqAssert = Value(5)
    val IsCallerOriginAssert = Value(6)
    val IsSignerOriginAssert = Value(7)
  }

  def parseBytes(context: ExecutionContext)
                (bytes: Array[Byte], data: Seq[DataEntry]): Either[ValidationError, OpcDiff] = bytes.head match {
    case opcType: Byte if opcType == AssertType.GteqZeroAssert.id && bytes.length == 2
      && bytes.tail.max < data.length && bytes.tail.min >= 0 => gtEq0(data(bytes(1)))
    case opcType: Byte if opcType == AssertType.LteqAssert.id && bytes.length == 3
      && bytes.tail.max < data.length && bytes.tail.min >= 0 => ltEq(data(bytes(1)), data(bytes(2)))
    case opcType: Byte if opcType == AssertType.LtInt64Assert.id && bytes.length == 2
      && bytes.tail.max < data.length && bytes.tail.min >= 0 => ltInt64(data(bytes(1)))
    case opcType: Byte if opcType == AssertType.GtZeroAssert.id && bytes.length == 2
      && bytes.tail.max < data.length && bytes.tail.min >= 0 => gt0(data(bytes(1)))
    case opcType: Byte if opcType == AssertType.EqAssert.id && bytes.length == 3
      && bytes.tail.max < data.length && bytes.tail.min >= 0 => eq(data(bytes(1)), data(bytes(2)))
    case opcType: Byte if opcType == AssertType.IsCallerOriginAssert.id && bytes.length == 2
      && bytes.tail.max < data.length && bytes.tail.min >= 0 => isCallerOrigin(context)(data(bytes(1)))
    case opcType: Byte if opcType == AssertType.IsSignerOriginAssert.id && bytes.length == 2
      && bytes.tail.max < data.length && bytes.tail.min >= 0 => isSignerOrigin(context)(data(bytes(1)))
    case _ => Left(ContractInvalidOPCode)
  }

}

