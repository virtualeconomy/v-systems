package vsys.blockchain.state.systemdiffs

import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.state.Diff
import vsys.blockchain.transaction.ValidationError

object SystemSendDiff {

  def send(context: ExecutionContext)
              (recipient: DataEntry, amount: DataEntry): Either[ValidationError, Diff] = {
    Right(Diff.empty)
  }
}
