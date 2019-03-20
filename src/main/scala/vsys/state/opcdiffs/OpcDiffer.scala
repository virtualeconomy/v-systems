package vsys.state.opcdiffs

import scorex.transaction.ValidationError
import vsys.contract.{ExecutionContext, DataEntry}


object OpcDiffer {

  object OpcType extends Enumeration {
    val AssertOpc = Value(1)
    val LoadOpc = Value(2)
    val TDBOpc = Value(3)
    val CDBOpc = Value(4)
  }

  def apply(context: ExecutionContext)
           (opc: Array[Byte],
            data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = opc.head match {

    case opcType: Byte if opcType == OpcType.AssertOpc.id =>
      AssertOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
     }

    case opcType: Byte if opcType == OpcType.LoadOpc.id =>
      LoadOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(data: Seq[DataEntry]) => Right((OpcDiff.empty, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.TDBOpc.id =>
      CDBOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.CDBOpc.id =>
      TDBOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

  }

}
