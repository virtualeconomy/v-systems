package vsys.blockchain.state.systemdiffs

object SystemDiffer {

  object SystemType extends Enumeration {
    val SystemSend = Value(1)
    val SystemDeposit = Value(2)
    val SystemWithdraw = Value(3)
    val SystemTransfer = Value(4)
  }

  object SystemFunction {
    val SystemSend: Array[Byte] = Array(SystemType.SystemSend.id.toByte)
    val SystemDeposit: Array[Byte] = Array(SystemType.SystemDeposit.id.toByte)
    val SystemWithdraw: Array[Byte] = Array(SystemType.SystemWithdraw.id.toByte)
    val SystemTransfer: Array[Byte] = Array(SystemType.SystemTransfer.id.toByte)
  }

  val SystemFunctions: Seq[Array[Byte]] = Seq(SystemFunction.SystemSend, SystemFunction.SystemDeposit,
    SystemFunction.SystemWithdraw, SystemFunction.SystemTransfer)
}
