package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.ContractContext
import vsys.state.opcdiffs.OpcFuncDiffer
import vsys.transaction.contract.ExecuteContractFunctionTransaction
import vsys.transaction.proof.{EllipticCurve25519Proof, Proofs}


object ExecuteContractFunctionTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: ExecuteContractFunctionTransaction): Either[ValidationError, Diff] = {
    if (tx.proofs.proofs.length > Proofs.MaxProofs) {
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    } else {
      val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
      val contractContext = ContractContext.fromExeConTx(s, height, tx).right.get
      val opcDiff = OpcFuncDiffer(contractContext)(tx.data).right.get
      Diff(height = height, tx = tx)
      Right(Diff(height = height, tx = tx,
        portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        contractDB = opcDiff.contractDB,
        contractTokens = opcDiff.contractTokens,
        tokenAccountBalance = opcDiff.tokenAccountBalance,
        chargedFee = tx.fee
      ))
    }
  }

}
