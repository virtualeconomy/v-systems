package vsys.state.opcdiffs

import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, ExecutionContext}


object OpcDiffer {

  object OpcType extends Enumeration {
    val AssertOpc = Value(1)
    val LoadOpc = Value(2)
    val CDBOpc = Value(3)
    val TDBOpc = Value(4)
    val TDBAOpc = Value(5)
    val ReturnOpc = Value(6)

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

    case opcType: Byte if opcType == OpcType.CDBOpc.id =>
      CDBOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.TDBOpc.id =>
      TDBOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.TDBAOpc.id =>
      TDBAOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.ReturnOpc.id => Left(GenericError("Invalid Opc type"))

    case _ => Left(GenericError("Invalid Opc type"))

  }

}
