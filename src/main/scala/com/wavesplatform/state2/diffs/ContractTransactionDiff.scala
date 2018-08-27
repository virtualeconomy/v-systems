package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vee.transaction.contract.{ChangeContractStatusAction, ChangeContractStatusTransaction, CreateContractTransaction}
import vee.transaction.proof.{EllipticCurve25519Proof, Proofs}

import scala.util.{Left, Right}

object ContractTransactionDiff {
  def create(s: StateReader, height: Int)(tx: CreateContractTransaction): Either[ValidationError, Diff] = {
    //no need to validate the name duplication coz that will create a duplicate transacion and
    // will fail with duplicated transaction id
    if (tx.proofs.proofs.length > Proofs.MaxProofs){
      Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
    }
    else {
      val sender = EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr).toOption.get.publicKey
      val contractInfo = (tx.contract.enabled, sender.toAddress, tx.contract.content)
      Right(Diff(height = height, tx = tx,
        portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
        contracts = Map(tx.contract.name -> contractInfo)
      ))
    }
  }

  def changeStatus(s: StateReader, height: Int)(tx: ChangeContractStatusTransaction): Either[ValidationError, Diff] = {
    val contractInfo = s.contractContent(tx.contractName) match {
      case None => Left(GenericError(s"Related Contract not found"))
      case Some(l) => Right(l)
    }
    for {
      contract <- contractInfo
      proof <- EllipticCurve25519Proof.fromBytes(tx.proofs.proofs.head.bytes.arr)
      sender = proof.publicKey
      originalAddr = contract._2
      originalEnabled = contract._1
      originalContent = contract._3
      newContractInfo = (!originalEnabled, sender.toAddress, originalContent)
      validation <- if (!originalAddr.equals(sender.toAddress.bytes)) {
        Left(GenericError(s"Only the creator of the contract can change contract status"))
      } else if (originalEnabled && (tx.action == ChangeContractStatusAction.Enable)) {
        Left(GenericError(s"The contract already enabled"))
      } else if (!originalEnabled && (tx.action == ChangeContractStatusAction.Disable)) {
        Left(GenericError(s"The contract already disabled"))
      } else if (tx.proofs.proofs.length > Proofs.MaxProofs) {
        Left(GenericError(s"Too many proofs, max ${Proofs.MaxProofs} proofs"))
      }else Right(())
    } yield Diff(height = height, tx = tx,
      portfolios = Map(sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      contracts = Map(tx.contractName -> newContractInfo)
    )
  }
}
