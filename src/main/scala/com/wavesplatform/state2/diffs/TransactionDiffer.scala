package com.wavesplatform.state2.diffs


import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.Diff
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError.UnsupportedTransactionType
import scorex.transaction._
//import scorex.transaction.assets._
//import scorex.transaction.assets.exchange.ExchangeTransaction
import vee.transaction.contract.{ChangeContractStatusTransaction, CreateContractTransaction}
import vee.transaction.database.DbPutTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vee.transaction.MintingTransaction
import vee.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError,tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(s: StateReader, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      t0 <- Signed.validateSignatures(tx)
      t1 <- CommonValidation.disallowTxFromFuture(currentBlockTimestamp, t0)
      t2 <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, t1)
      t4 <- CommonValidation.disallowDuplicateIds(s, settings, currentBlockHeight, t2)
      t5 <- CommonValidation.disallowSendingGreaterThanBalance(s, settings, currentBlockTimestamp, t4)
      t6 <- CommonValidation.disallowInvalidFeeScale(t5)
      diff <- t6 match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(settings, currentBlockHeight)(gtx)
        case ptx: PaymentTransaction => PaymentTransactionDiff(s, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
        //case itx: IssueTransaction => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
        //case rtx: ReissueTransaction => AssetTransactionsDiff.reissue(s, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
        //case btx: BurnTransaction => AssetTransactionsDiff.burn(s, currentBlockHeight)(btx)
        //case ttx: TransferTransaction => TransferTransactionDiff(s, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
        case ltx: LeaseTransaction => LeaseTransactionsDiff.lease(s, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction => LeaseTransactionsDiff.leaseCancel(s, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
        //case etx: ExchangeTransaction => ExchangeTransactionDiff(s, currentBlockHeight)(etx)
        //case atx: CreateAliasTransaction => CreateAliasTransactionDiff(currentBlockHeight)(atx)
        case mtx: MintingTransaction => MintingTransactionDiff(s, currentBlockHeight, settings, currentBlockTimestamp)(mtx)
        case cstx: ContendSlotsTransaction => ContendSlotsTransactionDiff(s,settings,currentBlockHeight)(cstx)
        case rstx: ReleaseSlotsTransaction => ReleaseSlotsTransactionDiff(s,settings,currentBlockHeight)(rstx)
        case cctx: CreateContractTransaction => ContractTransactionDiff.create(s, currentBlockHeight)(cctx)
        case ccstx: ChangeContractStatusTransaction => ContractTransactionDiff.changeStatus(s, currentBlockHeight)(ccstx)
        case dptx: DbPutTransaction => DbTransactionDiff.put(s, currentBlockHeight)(dptx)
        case _ => Left(UnsupportedTransactionType)
      }
      positiveDiff <- BalanceDiffValidation(s, currentBlockTimestamp)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
