package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.ContractUnsupportedOPC
import vsys.blockchain.contract.{DataEntry, ExecutionContext}

import scala.util.Try

object OpcDiffer {

  object OpcType extends Enumeration {
    val SystemOpc, AssertOpc, LoadOpc, CDBVOpc, CDBVROpc, TDBOpc, TDBROpc, TDBAOpc, TDBAROpc, ReturnOpc = Value
  }

  def apply(context: ExecutionContext)
           (opc: Array[Byte],
            data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = {
    opc.headOption.flatMap(f => Try(OpcType(f)).toOption) match {
      case Some(OpcType.SystemOpc) => opcDiffReturn(SystemTransferDiff.parseBytes(context)(opc.tail, data), data)
      case Some(OpcType.AssertOpc) => opcDiffReturn(AssertOpcDiff.parseBytes(context)(opc.tail, data), data)
      case Some(OpcType.LoadOpc) => seqDataEntryReturn(LoadOpcDiff.parseBytes(context)(opc.tail, data))
      case Some(OpcType.CDBVOpc) => opcDiffReturn(CDBVOpcDiff.parseBytes(context)(opc.tail, data), data)
      case Some(OpcType.CDBVROpc) => seqDataEntryReturn(CDBVROpcDiff.parseBytes(context)(opc.tail, data))
      case Some(OpcType.TDBOpc) => opcDiffReturn(TDBOpcDiff.parseBytes(context)(opc.tail, data), data)
      case Some(OpcType.TDBROpc) => seqDataEntryReturn(TDBROpcDiff.parseBytes(context)(opc.tail, data))
      case Some(OpcType.TDBAOpc) => opcDiffReturn(TDBAOpcDiff.parseBytes(context)(opc.tail, data), data)
      case Some(OpcType.TDBAROpc) => seqDataEntryReturn(TDBAROpcDiff.parseBytes(context)(opc.tail, data))
      case Some(OpcType.ReturnOpc) => seqDataEntryReturn(ReturnOpcDiff.parseBytes(context)(opc.tail, data))
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
