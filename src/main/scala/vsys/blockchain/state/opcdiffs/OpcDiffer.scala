package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.ContractUnsupportedOPC
import vsys.blockchain.contract.{DataEntry, ExecutionContext}


object OpcDiffer {

  object OpcType extends Enumeration(1) {
    val AssertOpc, LoadOpc, CDBVOpc, CDBVROpc, TDBOpc, TDBROpc, TDBAOpc, TDBAROpc, ReturnOpc = Value
  }

  def apply(context: ExecutionContext)
           (opc: Array[Byte],
            data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = {
    val assertOpcId = OpcType.AssertOpc.id.toByte
    val loadOpcId = OpcType.LoadOpc.id.toByte
    val cdbvOpcId = OpcType.CDBVOpc.id.toByte
    val cdbvrOpcId = OpcType.CDBVROpc.id.toByte
    val tdbOpcId = OpcType.TDBOpc.id.toByte
    val tdbrOpcId = OpcType.TDBROpc.id.toByte
    val tdbaOpcId = OpcType.TDBAOpc.id.toByte
    val tdbarOpcId = OpcType.TDBAROpc.id.toByte
    val returnOpcId = OpcType.ReturnOpc.id.toByte
    opc.headOption match {
      case Some(`assertOpcId`) => opcDiffReturn(AssertOpcDiff.parseBytes(context)(opc.tail, data), data)
      case Some(`loadOpcId`) => seqDataEntryReturn(LoadOpcDiff.parseBytes(context)(opc.tail, data))
      case Some(`cdbvOpcId`) => opcDiffReturn(CDBVOpcDiff.parseBytes(context)(opc.tail, data), data)
      case Some(`cdbvrOpcId`) => seqDataEntryReturn(CDBVROpcDiff.parseBytes(context)(opc.tail, data))
      case Some(`tdbOpcId`) => opcDiffReturn(TDBOpcDiff.parseBytes(context)(opc.tail, data), data)
      case Some(`tdbrOpcId`) => seqDataEntryReturn(TDBROpcDiff.parseBytes(context)(opc.tail, data))
      case Some(`tdbaOpcId`) => opcDiffReturn(TDBAOpcDiff.parseBytes(context)(opc.tail, data), data)
      case Some(`tdbarOpcId`) => seqDataEntryReturn(TDBAROpcDiff.parseBytes(context)(opc.tail, data))
      case Some(`returnOpcId`) => seqDataEntryReturn(ReturnOpcDiff.parseBytes(context)(opc.tail, data))
      case _ => Left(ContractUnsupportedOPC)
    }
  }

  private def seqDataEntryReturn(res: Either[ValidationError, Seq[DataEntry]]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = {
    res match {
      case Right(d: Seq[DataEntry]) => Right((OpcDiff.empty, d))
      case Left(validationError: ValidationError) => Left(validationError)
    }
  }

  private def opcDiffReturn(res: Either[ValidationError, OpcDiff], data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = {
    res match {
      case Right(opcDiff: OpcDiff) => Right((opcDiff, data))
      case Left(validationError: ValidationError) => Left(validationError)
    }
  }

}
