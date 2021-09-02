package vsys.blockchain.contract.vstableswap

import vsys.blockchain.contract.ContractGenHelper._
import vsys.blockchain.contract.DataType
import vsys.blockchain.contract.ContractVStableSwap._

trait VStableSwapFunction {

  val initId: Short = 0

  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte, DataType.TokenId.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte)

  val initWrongDataType: Array[Byte] = Array(DataType.TokenId.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte)

  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(5.toByte),
    cdbvSet ++ Array(makerStateVar.index, 5.toByte),
    cdbvSet ++ Array(baseTokenIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(targetTokenIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(maxOrderPerUserStateVar.index, 2.toByte),
    cdbvSet ++ Array(unitPriceBaseStateVar.index, 3.toByte),
    cdbvSet ++ Array(unitPriceTargetStateVar.index, 4.toByte)
  )

  val initWrongTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(5.toByte),
    cdbvSet ++ Array(makerStateVar.index, 5.toByte),
    cdbvSet ++ Array(baseTokenIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(targetTokenIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(maxOrderPerUserStateVar.index, 2.toByte),
    cdbvSet ++ Array(unitPriceBaseStateVar.index, 3.toByte),
    Array(5.toByte, 3.toByte) ++ Array(unitPriceTargetStateVar.index, 4.toByte)
  )

  val nonReturnType: Array[Byte] = Array[Byte]()
  val onInitTriggerType: Byte = 0

  lazy val wrongDataTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initWrongDataType, initTriggerOpcs))
  lazy val wrongOpcTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initWrongTriggerOpcs))

}
