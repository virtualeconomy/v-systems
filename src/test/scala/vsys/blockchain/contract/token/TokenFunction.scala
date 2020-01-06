package vsys.blockchain.contract.token

import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract.DataType

trait TokenFunction {

  val initId: Short = 0

  val initDataType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.ShortText.id.toByte)

  val initWrongDataType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Int32.id.toByte, DataType.ShortText.id.toByte)

  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSinger ++ Array(3.toByte),
    cdbvSet ++ Array(0.toByte, 3.toByte),
    cdbvSet ++ Array(1.toByte, 3.toByte),
    tdbNewToken ++ Array(0.toByte, 1.toByte, 2.toByte)
  )

  val initWrongTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSinger ++ Array(3.toByte),
    cdbvSet ++ Array(0.toByte, 3.toByte),
    cdbvSet ++ Array(1.toByte, 3.toByte),
    Array(5.toByte, 3.toByte) ++ Array(0.toByte, 1.toByte, 2.toByte)
  )

  val nonReturnType: Array[Byte] = Array[Byte]()
  val onInitTriggerType: Byte = 0

  lazy val wrongDataTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initWrongDataType, initTriggerOpcs))
  lazy val wrongOpcTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initWrongTriggerOpcs))

}
