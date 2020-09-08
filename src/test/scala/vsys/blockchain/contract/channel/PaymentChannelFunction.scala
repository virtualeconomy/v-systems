package vsys.blockchain.contract.channel

import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract.DataType

trait PaymentChannelFunction {

  val initId: Short = 0

  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte)

  val initWrongDataType: Array[Byte] = Array(DataType.Amount.id.toByte)

  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(1.toByte),
    cdbvSet ++ Array(0.toByte, 1.toByte),
    cdbvSet ++ Array(1.toByte, 0.toByte)
  )

  val initWrongTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(1.toByte),
    cdbvSet ++ Array(0.toByte, 1.toByte),
    Array(5.toByte, 3.toByte) ++ Array(1.toByte, 0.toByte)
  )

  val nonReturnType: Array[Byte] = Array[Byte]()
  val onInitTriggerType: Byte = 0

  lazy val wrongDataTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initWrongDataType, initTriggerOpcs))
  lazy val wrongOpcTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initWrongTriggerOpcs))

}
