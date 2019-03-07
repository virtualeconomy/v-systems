package vsys.state.opcdiffs

import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError
import vsys.contract.DataEntry
import vsys.state.opcdiffs.AssertOpcDiff.AssertType
import vsys.state.opcdiffs.AssertOpcDiff._

object OpcDiffer {

  object OpcType extends Enumeration {
    val AssertOpc = Value(1)
    val LoadOpc = Value(2)
  }

  def apply(s: StateReader)(opc: Array[Byte], dataStack: Seq[DataEntry]): Either[ValidationError, OpcDiff] = opc.head match {
    case opcType: Byte if opcType == OpcType.AssertOpc.id => opc(1) match {
      case assertType: Byte if assertType == AssertType.GteqZeroAssert.id =>
        gteq0(dataStack(opc(2)))
    }

    case opcType: Byte if opcType == OpcType.LoadOpc.id => opc(1) match {
      case assertType: Byte if assertType == AssertType.GteqZeroAssert.id =>
        gteq0(dataStack(opc(2)))
    }

  }
}
