package vsys.blockchain.state.systemdiffs

import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.state.Diff
import vsys.blockchain.state.systemdiffs.SystemDiffer.{SystemFunction, SystemType}
import vsys.blockchain.transaction.ValidationError

object SystemFuncDiffer {

  def apply(executionContext: ExecutionContext)
           (data: Seq[DataEntry]): Either[ValidationError, Diff] = {
    val sysFunc = executionContext.opcFunc
    sysFunc match {
      case SystemFunction.SystemSend => Right(Diff.empty)
      case SystemFunction.SystemDeposit => Right(Diff.empty)
      case SystemFunction.SystemWithdraw => Right(Diff.empty)
      case SystemFunction.SystemTransfer => Right(Diff.empty)
    }
  }
}
