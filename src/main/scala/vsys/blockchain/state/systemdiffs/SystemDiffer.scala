package vsys.blockchain.state.systemdiffs

import vsys.blockchain.contract.{DataEntry, ExecutionContext}
import vsys.blockchain.state.opcdiffs.OpcDiff
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.ContractInvalidFunction

object SystemDiffer {

  object SystemType extends Enumeration {
    val SysAssert = Value(1)
    val SysLoad = Value(2)
    val SysTransfer = Value(3)
  }

  def apply(context: ExecutionContext)
           (opc: Array[Byte],
            data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = opc.head match {

    case _ => Left(ContractInvalidFunction)
  }
}
