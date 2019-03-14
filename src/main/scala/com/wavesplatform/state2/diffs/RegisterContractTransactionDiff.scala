package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteStr, Diff, LeaseInfo, Portfolio}
import scorex.serialization.Deser
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.ExecutionContext
import vsys.state.opcdiffs.OpcFuncDiffer
import vsys.transaction.contract.RegisterContractTransaction
import vsys.transaction.proof.{EllipticCurve25519Proof, Proofs}

import scala.util.{Left, Right}

object RegisterContractTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: RegisterContractTransaction): Either[ValidationError, Diff] = {
    //no need to validate the name duplication coz that will create a duplicate transacion and
    // will fail with duplicated transaction id
    if (tx.proofs.proofs.length > Proofs.MaxProofs){
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    }
    else {
      val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
      val contractInfo = (height, tx.contract, Set(sender.toAddress))
//      for {
//        exContext <- ExecutionContext.fromRegConTx(s, height, tx)
//        opcDiff <- OpcFuncDiffer(exContext)(tx.data)
//        diff = opcDiff.asTransactionDiff(height, tx)
//      } yield Diff(height = height, tx = tx,
//        portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
//        contracts = Map(tx.contractId.bytes -> contractInfo),
//        chargedFee = tx.fee
//      ).combine(diff)
      Right(Diff(height = height, tx = tx,
        portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        contracts = Map(tx.contractId.bytes -> contractInfo),
        chargedFee = tx.fee
      ))
    }
  }

}
