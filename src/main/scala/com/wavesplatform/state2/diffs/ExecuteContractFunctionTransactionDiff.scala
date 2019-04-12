package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.ExecutionContext
import vsys.state.opcdiffs.OpcFuncDiffer
import vsys.transaction.TransactionStatus
import vsys.transaction.contract.ExecuteContractFunctionTransaction
import vsys.transaction.proof.{EllipticCurve25519Proof, Proofs}


object ExecuteContractFunctionTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: ExecuteContractFunctionTransaction): Either[ValidationError, Diff] = {
    if (tx.proofs.proofs.length > Proofs.MaxProofs) {
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    } else {
      val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
      (for {
        exContext <- ExecutionContext.fromExeConTx(s, height, tx)
        diff <- OpcFuncDiffer(exContext)(tx.data)
      } yield diff) match {
        case Left(_) => Right(Diff(height = height, tx = tx,
          portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
          chargedFee = tx.fee, txStatus = TransactionStatus.ExecuteContractFunctionFailed))
        case Right(df) => Right(Diff(height = height, tx = tx,
          portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)), tokenDB = df.tokenDB,
          tokenAccountBalance = df.tokenAccountBalance, contractDB = df.contractDB, contractTokens = df.contractTokens,
          relatedAddress = df.relatedAddress, chargedFee = tx.fee))
      }
    }
  }

}
