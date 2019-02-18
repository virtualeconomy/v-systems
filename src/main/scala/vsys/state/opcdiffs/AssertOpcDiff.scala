package vsys.state.opcdiffs

import com.wavesplatform.state2._
import scorex.account.Address
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError

import scala.util.{Left, Right}

object AssertOpcDiff {

  def gteq0(v: Long): Either[ValidationError, Diff] = {
    if (v >= 0)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (gteq0): Value $v is negative"))
  }

  def lteq(v1: Long, v2: Long): Either[ValidationError, Diff] = {
    if (v1 <= v2)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (lteq0): Value $v2 is larger than $v1"))
  }

  def ltint64(m: Long): Either[ValidationError, Diff] = {
    if (m <= Long.MaxValue)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (ltint64): Value $m is invalid"))
  }

  def gt0(v: Long): Either[ValidationError, Diff] = {
    if (v > 0)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (gt0): Value $v is non-positive"))
  }

  def eq(add1: Address, add2: Address): Either[ValidationError, Diff] = {
    if (add1 == add2)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (eq): Address ${add1.address} is not equal to ${add2.address}"))
  }

  def eq(v1: Long, v2: Long): Either[ValidationError, Diff] = {
    if (v1 == v2)
      Right(Diff.empty)
    else
      Left(GenericError(s"Invalid Assert (eq): Address $v1 is not equal to $v2"))
  }

}

