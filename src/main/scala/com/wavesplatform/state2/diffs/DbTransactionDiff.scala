package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.transaction.database.DbPutTransaction
import vsys.transaction.proof.{EllipticCurve25519Proof, Proofs}

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
