package vsys.state.opcdiffs

import com.google.common.primitives.Longs
import com.wavesplatform.state2.ByteStr
import scorex.account.Address
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
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
      Left(GenericError(s"Invalid Assert (eq): DataEntry $add1 is not equal to $add2"))
  }

  def isOrigin(context: ExecutionContext)(stateVarIssuer: Array[Byte]): Either[ValidationError, OpcDiff] = {
    val issuer = context.state.contractInfo(ByteStr(context.contractId.bytes.arr ++ Array(stateVarIssuer(0))))
    val signer = context.signers.head
    if (issuer.isEmpty)
      Left(GenericError("Issuer not defined"))
    else if (issuer.get.bytes sameElements signer.bytes.arr)
      Left(GenericError(s"Address $issuer does not equal $signer 's address"))
    else
      Right(OpcDiff.empty)
  }

  object AssertType extends Enumeration {
    val GteqZeroAssert = Value(1)
    val LteqAssert = Value(2)
    val LtInt64Assert = Value(3)
    val Gt0Assert = Value(4)
    val EqAssert = Value(5)
    val isOriginAssert = Value(6)
  }


}

