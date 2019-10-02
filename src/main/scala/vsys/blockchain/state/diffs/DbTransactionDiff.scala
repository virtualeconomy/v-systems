package vsys.blockchain.state.diffs

import vsys.account.Address
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.database.DbPutTransaction

import scala.util.{Left, Right}

object DbTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: DbPutTransaction): Either[ValidationError, Diff] = {
    for {
      proof <- tx.proofs.firstCurveProof
      senderAddress: Address = proof.publicKey.toAddress
      _ <- if (tx.dbKey.length > DbPutTransaction.MaxDbKeyLength ||
               tx.dbKey.length < DbPutTransaction.MinDbKeyLength) {
        Left(ValidationError.InvalidDbKey)
      } else Right(())
    } yield Diff(
      height = height,
      tx = tx,
      portfolios = Map(senderAddress -> Portfolio(-tx.transactionFee, LeaseInfo.empty, Map.empty)),
      dbEntries = Map(tx.storageKey -> tx.entry),
      chargedFee = tx.transactionFee
    )
  }
}
