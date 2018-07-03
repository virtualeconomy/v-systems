package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.contract.{ChangeContractStatusAction, ChangeContractStatusTransaction, CreateContractTransaction}

import scala.util.{Left, Right}

object ContractTransactionDiff {
  def create(s: StateReader, height: Int)(tx: CreateContractTransaction): Either[ValidationError, Diff] = {
    //no need to validate the name duplication coz that will create a duplicate transacion and
    // will fail with duplicated transaction id
    Right(Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      contracts = Map(tx.contract.name -> (tx.contract.enabled, tx.sender.toAddress, tx.contract.content))
    ))
  }

  def changeStatus(s: StateReader, height: Int)(tx: ChangeContractStatusTransaction): Either[ValidationError, Diff] = {
    val contractInfo = s.contractContent(tx.contractName) match {
      case None => Left(GenericError(s"Related Contract not found"))
      case Some(l) => Right(l)
    }
    for {
      contract <- contractInfo
      originalAddr = contract._2
      originalEnabled = contract._1
      originalContent = contract._3
      validation <- if (!originalAddr.equals(tx.sender.toAddress.bytes)) {
        Left(GenericError(s"Only the creater of the contract can change contract status"))
      } else if (originalEnabled && (tx.action == ChangeContractStatusAction.Enable)) {
        Left(GenericError(s"The contract already enabled"))
      } else if (!originalEnabled && (tx.action == ChangeContractStatusAction.Disable)) {
        Left(GenericError(s"The contract already disabled"))
      } else Right(())
    } yield Diff(height = height, tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseInfo.empty, Map.empty)),
      contracts = Map(tx.contractName -> (!originalEnabled, tx.sender.toAddress, originalContent))
    )
  }
}
