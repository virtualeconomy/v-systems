package vsys.blockchain.transaction

import com.typesafe.config.ConfigException.BadValue
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.settings.{FeesSettings, FeeSettings}

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
class FeeCalculator(settings: FeesSettings) {
  // txType -> txMinFee
  private val map: Map[Int, Long] = {
    settings.fees.flatMap { case (transactionType: Int, feeSettingsList: List[FeeSettings]) =>
      feeSettingsList.map { v =>
        if (v.asset.toUpperCase != "VSYS") throw new BadValue(v.asset, s"Unsupported fee type", new UnsupportedOperationException())
        transactionType -> v.fee
      }
    }
  }

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    map.get(tx.transactionType.id) match {
      case Some(minimumFee) =>
        if (minimumFee <= tx.transactionFee) {
          Right(tx)
        } else {
          Left(GenericError(s"Fee for ${tx.transactionType} transaction does not exceed minimal value of $minimumFee"))
        }
      case None =>
        Left(GenericError(s"Minimum fee is not defined"))
    }
  }
}
