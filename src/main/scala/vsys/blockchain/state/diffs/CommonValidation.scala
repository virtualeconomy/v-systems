package vsys.blockchain.state.diffs

import vsys.blockchain.contract._
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction.ValidationError.{GenericError, Mistiming}
import vsys.blockchain.transaction._
import vsys.blockchain.transaction.lease._
import vsys.blockchain.transaction.proof.Proofs
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.spos._
import vsys.blockchain.transaction.database._
import vsys.settings.FunctionalitySettings

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours

  def disallowSendingGreaterThanBalance[T <: Transaction](s: StateReader, settings: FunctionalitySettings, blockTime: Long, tx: T): Either[ValidationError, T] =
    tx match {
      case ptx: PaymentTransaction =>
        for {
          proof <- ptx.proofs.firstCurveProof
          sender = proof.publicKey
          _ <- if(s.accountPortfolio(sender).balance < (ptx.amount + ptx.transactionFee))
            Left(GenericError(s"Attempt to pay unavailable funds: balance " +
              s"${s.accountPortfolio(sender).balance} is less than ${ptx.amount + ptx.transactionFee}"))
            else Right(())
        } yield tx
      case _ => Right(tx)
    }

  def disallowDuplicateIds[T <: Transaction](state: StateReader, settings: FunctionalitySettings, height: Int, tx: T): Either[ValidationError, T] = {
    if (state.containsTransaction(tx.id))
        Left(GenericError(s"Tx id cannot be duplicated. Current height is: $height. Tx with such id already present"))
    else Right(tx)
  }

  private def disallowInvalidContractTxs[T <: Transaction](settings: FunctionalitySettings, h: Int, tx: T, c: Contract): Either[ValidationError, T] = {
    if (h <= settings.allowContractTransactionAfterHeight)
      Left(GenericError(s"must not appear before height=${settings.allowContractTransactionAfterHeight}"))
    else if (h <= settings.allowDepositWithdrawContractAfterHeight &&
       (c != ContractPermitted.contract &&
        c != ContractPermitted.contractWithoutSplit))
      Left(GenericError(s"deposit withdraw contracts must not appear before height=${settings.allowDepositWithdrawContractAfterHeight}"))
    else Right(tx)
  }

  def disallowBeforeActivationHeight[T <: Transaction](settings: FunctionalitySettings, h: Int, tx: T): Either[ValidationError, T] =
    tx match {
      case t: RegisterContractTransaction => disallowInvalidContractTxs(settings, h, tx, t.contract)
      case _: ExecuteContractFunctionTransaction if h <= settings.allowContractTransactionAfterHeight =>
        Left(GenericError(s"must not appear before time=${settings.allowContractTransactionAfterHeight}"))
      case _: GenesisTransaction => Right(tx)
      case _: PaymentTransaction => Right(tx)
      case _: LeaseTransaction => Right(tx)
      case _: LeaseCancelTransaction => Right(tx)
      case _: MintingTransaction => Right(tx)
      case _: ContendSlotsTransaction => Right(tx)
      case _: ReleaseSlotsTransaction => Right(tx)
      case _: ExecuteContractFunctionTransaction => Right(tx)
      case _: DbPutTransaction => Right(tx)
      case _ => Left(GenericError("Unknown transaction must be explicitly registered within ActivatedValidator"))
    }

  def disallowTxFromFuture[T <: Transaction](time: Long, tx: T): Either[ValidationError, T] = {
    if ((tx.timestamp - time) > MaxTimeTransactionOverBlockDiff.toNanos)
      Left(Mistiming(s"Transaction ts ${tx.timestamp} is from far future. BlockTime: $time"))
    else Right(tx)
  }

  def disallowTxFromPast[T <: Transaction](prevBlockTime: Option[Long], tx: T): Either[ValidationError, T] =
    prevBlockTime match {
      case Some(t) if (t - tx.timestamp) > MaxTimePrevBlockOverTransactionDiff.toNanos =>
        Left(Mistiming(s"Transaction ts ${tx.timestamp} is too old. Previous block time: $prevBlockTime"))
      case _ => Right(tx)
    }

  def disallowInvalidFeeScale[T <: Transaction](tx: T): Either[ValidationError, T] = {
    if (tx.feeScale != 100){
      Left(ValidationError.WrongFeeScale(tx.feeScale))
    } else Right(tx)
  }

  def disallowProofsCountOverflow[T <: ProvenTransaction](tx: T): Either[ValidationError, T] = {
    if (tx.proofs.proofs.length > Proofs.MaxProofs){
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    } else Right(tx)
  }
}


