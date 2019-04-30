package vsys.state.opcdiffs

import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.ContractUnsupportedOPC
import vsys.contract.{DataEntry, ExecutionContext}


object OpcDiffer {

  object OpcType extends Enumeration {
    val AssertOpc = Value(1)
    val LoadOpc = Value(2)
    val CDBVOpc = Value(3)
    val CDBVROpc = Value(4)
    val TDBOpc = Value(5)
    val TDBROpc = Value(6)
    val TDBAOpc = Value(7)
    val TDBAROpc = Value(8)
    val ReturnOpc = Value(9)

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
        case Right(d: Seq[DataEntry]) => Right((OpcDiff.empty, d))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.CDBVOpc.id =>
      CDBVOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.CDBVROpc.id =>
      CDBVROpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(d: Seq[DataEntry]) => Right((OpcDiff.empty, d))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.TDBOpc.id =>
      TDBOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.TDBROpc.id =>
      TDBROpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(d: Seq[DataEntry]) => Right((OpcDiff.empty, d))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.TDBAOpc.id =>
      TDBAOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.TDBAROpc.id =>
      TDBAROpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(d: Seq[DataEntry]) => Right((OpcDiff.empty, d))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case opcType: Byte if opcType == OpcType.ReturnOpc.id =>
      ReturnOpcDiff.parseBytes(context)(opc.tail, data) match {
        case Right(d: Seq[DataEntry]) => Right((OpcDiff.empty, d))
        case Left(validationError: ValidationError) => Left(validationError)
      }

    case _ => Left(ContractUnsupportedOPC)

  }

}
