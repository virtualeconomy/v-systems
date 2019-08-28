package vsys.blockchain.state.systemdiffs

import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.state.Diff
import vsys.blockchain.transaction.ValidationError

object SystemWithdrawDiff {

  def withdraw(context: ExecutionContext)
              (fromAccount: DataEntry, amount: DataEntry): Either[ValidationError, Diff] = {
    Right(Diff.empty)
  }
}
