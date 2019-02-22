package vsys.state.opcdiffs

import com.google.common.primitives.Longs
import com.wavesplatform.state2._
import scorex.account.Address
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}

import scala.util.{Left, Right}

object AssertOpcDiff {

  def gteq0(v: DataEntry): Either[ValidationError, Diff] = {
    if (v.dataType == DataType.Amount && Longs.fromByteArray(v.data) >= 0)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (gteq0): Value ${Longs.fromByteArray(v.data)} is negative"))
  }

  def lteq(v1: DataEntry, v2: DataEntry): Either[ValidationError, Diff] = {
    if (v1.dataType == DataType.Amount && v2.dataType == DataType.Amount
      && Longs.fromByteArray(v1.data) <= Longs.fromByteArray(v2.data))
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (lteq0): Value ${Longs.fromByteArray(v2.data)} is larger than $v1"))
  }

  def ltint64(m: DataEntry): Either[ValidationError, Diff] = {
    if (m.dataType == DataType.Amount && Longs.fromByteArray(m.data) <= Long.MaxValue)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (ltint64): Value ${Longs.fromByteArray(m.data)} is invalid"))
  }

  def gt0(v: DataEntry): Either[ValidationError, Diff] = {
    if (v.dataType == DataType.Amount && Longs.fromByteArray(v.data) > 0)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (gt0): Value $v is non-positive"))
  }

  def eq(add1: DataEntry, add2: DataEntry): Either[ValidationError, Diff] = {
    if (add1.dataType == DataType.Address && add2.dataType == DataType.Address
      && Address.fromBytes(add1.data) == Address.fromBytes(add2.data))
      Right(Diff.empty)
    else if (add1.dataType == DataType.Amount && add2.dataType == DataType.Amount
      && Longs.fromByteArray(add1.data) == Longs.fromByteArray(add2.data))
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (eq): DataEntry $add1 is not equal to $add2"))
  }


}

