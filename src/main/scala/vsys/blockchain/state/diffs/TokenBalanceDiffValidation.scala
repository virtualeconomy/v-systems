package vsys.blockchain.state.diffs

import cats.implicits._
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{ByteStr, Diff}
import vsys.blockchain.transaction.{Transaction, ValidationError}
import vsys.blockchain.transaction.ValidationError.GenericError

import scala.util.{Left, Right}

object TokenBalanceDiffValidation {

  def apply[T <: Transaction](s: StateReader, time: Long)(d: Diff): Either[ValidationError, Diff] = {

    val changedKeys = d.tokenAccountBalance.keySet
    val positiveBalanceErrors: Map[ByteStr, String] = changedKeys.flatMap(tokenKey => {

      val oldTokenBalance = s.tokenAccountBalance(tokenKey)
      val tokenBalanceDiff = d.tokenAccountBalance(tokenKey)
      val newTokenBalance = oldTokenBalance.combine(tokenBalanceDiff)

      val err = if (newTokenBalance < 0) {
        Some(s"negative token/total balance: $tokenKey, old: $oldTokenBalance, new: $newTokenBalance")
      } else None
      err.map(tokenKey -> _)
    }).toMap

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(GenericError(positiveBalanceErrors.values.mkString(", ")))
    }
  }

}
