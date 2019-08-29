package vsys.blockchain.state.systemdiffs

import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.state.opcdiffs.OpcDiff
import vsys.blockchain.transaction.ValidationError

object SystemTransferDiff {

  def transfer(context: ExecutionContext)
              (sender: DataEntry, recipient: DataEntry, amount: DataEntry): Either[ValidationError, OpcDiff] = {
    Right(OpcDiff.empty)
  }
}