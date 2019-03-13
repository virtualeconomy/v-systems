package vsys.state.opcdiffs

import scorex.transaction.ValidationError
import vsys.contract.{ExecutionContext, DataEntry}
import vsys.state.opcdiffs.AssertOpcDiff.AssertType
import vsys.state.opcdiffs.AssertOpcDiff._

object OpcDiffer {

  object OpcType extends Enumeration {
    val AssertOpc = Value(1)
    val LoadOpc = Value(2)
  }

  def apply(contractContext: ExecutionContext)
           (opc: Array[Byte],
            data: Seq[DataEntry]): Either[ValidationError, (OpcDiff, Seq[DataEntry])] = opc.head match {
    case opcType: Byte if opcType == OpcType.AssertOpc.id => opc(1) match {
      case assertType: Byte if assertType == AssertType.GteqZeroAssert.id =>
        Right((gtEq0(data(opc(2))).right.get, data))
    }

    case opcType: Byte if opcType == OpcType.LoadOpc.id => opc(1) match {
      case assertType: Byte if assertType == AssertType.GteqZeroAssert.id =>
        Right((gtEq0(data(opc(2))).right.get, data))
    }

  }
}
