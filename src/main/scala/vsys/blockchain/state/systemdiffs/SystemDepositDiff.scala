package vsys.blockchain.state.systemdiffs

import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.state.Diff
import vsys.blockchain.transaction.ValidationError

object SystemDepositDiff {

  def deposit(context: ExecutionContext)
             (toAccount: DataEntry, amount: DataEntry): Either[ValidationError, Diff] = {
    Right(Diff.empty)
  }
}
