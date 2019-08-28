package vsys.blockchain.state.systemdiffs

object SystemDiffer {

  object SystemType extends Enumeration {
    val SystemSend = Value(1)
    val SystemDeposit = Value(2)
    val SystemWithdraw = Value(3)
    val SystemTransfer = Value(4)

  }

  val SystemFunction: Seq[Array[Byte]] = Seq(SystemSend, SystemDeposit, SystemWithdraw, SystemTransfer)
  lazy val SystemSend: Array[Byte] = Array.emptyByteArray
  lazy val SystemDeposit: Array[Byte] = Array.emptyByteArray
  lazy val SystemWithdraw: Array[Byte] = Array.emptyByteArray
  lazy val SystemTransfer: Array[Byte] = Array.emptyByteArray

}
