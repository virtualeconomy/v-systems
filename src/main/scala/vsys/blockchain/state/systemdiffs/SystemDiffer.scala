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

    case opcType: Byte if opcType == SystemType.SysAssert.id =>
      SystemAssertDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }
    case opcType: Byte if opcType == SystemType.SysLoad.id =>
      SystemLoadDiff.parseBytes(context)(opc.tail, data) match {
        case Right(d: Seq[DataEntry]) => Right((OpcDiff.empty, d))
        case Left(validationError: ValidationError) => Left(validationError)
      }
    case opcType: Byte if opcType == SystemType.SysTransfer.id =>
      SystemTransferDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }
    case _ => Left(ContractInvalidFunction)
  }
}
