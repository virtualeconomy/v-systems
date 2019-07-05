package vsys.blockchain.state.diffs

import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.GenericError
import vsys.blockchain.transaction.database.DbPutTransaction
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}

import scala.util.{Left, Right}

object DbTransactionDiff {
  def put(s: StateReader, height: Int)(tx: DbPutTransaction): Either[ValidationError, Diff] = {

    val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
    val proofLength = tx.proofs.proofs.length
    // any validation needed? maybe later access control?
    if (proofLength > Proofs.MaxProofs){
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    } else if (tx.dbKey.length > DbPutTransaction.MaxDbKeyLength || tx.dbKey.length < DbPutTransaction.MinDbKeyLength){
      Left(ValidationError.InvalidDbKey)
    } else {
      Right(Diff(height = height, tx = tx,
        portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        dbEntries = Map(tx.storageKey -> tx.entry),
        chargedFee = tx.fee
      ))
    }
  }

}
