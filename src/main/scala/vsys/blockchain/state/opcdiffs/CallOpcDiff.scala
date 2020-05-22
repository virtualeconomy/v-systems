package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.utils.ScorexLogging
import vsys.account.ContractAccount
import vsys.blockchain.contract.{CallType, DataEntry, ExecutionContext}


object CallOpcDiff extends ScorexLogging {

  def apply(executionContext: ExecutionContext,
            diff: OpcDiff,
            contractId: ContractAccount,
            data: Seq[DataEntry],
            callType: CallType.Value, // trigger or function
            callIndex: Int // trigger type or function index
           ): Either[ValidationError, OpcDiff] = {
    for {
      callContext <- ExecutionContext.fromCallOpc(executionContext, diff, contractId, callType, callIndex)
      diff <- OpcFuncDiffer(callContext)(data)
    } yield diff
  }

}