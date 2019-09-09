package vsys.blockchain.state.diffs

import vsys.account.Address
import vsys.blockchain.contract.ExecutionContext
import vsys.blockchain.state.reader.StateReader
import vsys.blockchain.state.{Diff, LeaseInfo, Portfolio}
import vsys.blockchain.state.opcdiffs.OpcFuncDiffer
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.{TransactionStatus, ValidationError}
import vsys.blockchain.transaction.ValidationError._

object RegisterContractTransactionDiff {
  def apply(s: StateReader, height: Int)(tx: RegisterContractTransaction): Either[ValidationError, Diff] = {
    /**
      no need to validate the name duplication coz that will create a duplicate transacion and
      will fail with duplicated transaction id
    */
    tx.proofs.firstCurveProof.flatMap( proof => {
      val senderAddr: Address = proof.publicKey
      val contractInfo = (height, tx.id, tx.contract, Set(senderAddr))
      ( for {
        exContext <- ExecutionContext.fromRegConTx(s, height, tx)
        diff <- OpcFuncDiffer(exContext)(tx.data)
      } yield Diff(
        height = height,
        tx = tx,
        portfolios = Map(senderAddr -> Portfolio(-tx.transactionFee, LeaseInfo.empty, Map.empty)),
        contracts = Map(tx.contractId.bytes -> contractInfo),
        contractDB = diff.contractDB,
        contractTokens = diff.contractTokens,
        tokenDB = diff.tokenDB,
        tokenAccountBalance = diff.tokenAccountBalance,
        relatedAddress = diff.relatedAddress,
        chargedFee = tx.transactionFee
      ))
      .left.flatMap( e =>
        Right(Diff(
          height = height,
          tx = tx,
          portfolios = Map(senderAddr -> Portfolio(-tx.transactionFee, LeaseInfo.empty, Map.empty)),
          chargedFee = tx.transactionFee,
          txStatus = e match {
            case ce: ContractValidationError  => ce.transactionStatus
            case _ => TransactionStatus.Failed
          }
        ))
      )
    })
  }
}
