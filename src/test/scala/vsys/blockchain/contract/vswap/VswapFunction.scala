package vsys.blockchain.contract.vswap

import vsys.blockchain.contract.ContractGen.{basicConstantGet, cdbvSet, getFunctionBytes, loadSigner}
import vsys.blockchain.contract.ContractVSwap._
import vsys.blockchain.contract.{DataEntry, DataType}

trait VswapFunction {

  val initId: Short = 0

  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte, DataType.TokenId.id.toByte, DataType.TokenId.id.toByte, DataType.Amount.id.toByte)
  val initWrongDataType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.TokenId.id.toByte, DataType.TokenId.id.toByte, DataType.Amount.id.toByte)

  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(4.toByte),
    cdbvSet ++ Array(makerStateVar.index, 4.toByte),
    cdbvSet ++ Array(tokenAIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(tokenBIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(tokenLiquidityIdStateVar.index, 2.toByte),
    cdbvSet ++ Array(minimumLiquidityStateVar.index, 3.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    cdbvSet ++ Array(swapStatusStateVar.index, 5.toByte)
  )

  val initWrongTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(4.toByte),
    Array(5.toByte, 3.toByte) ++ Array(makerStateVar.index, 4.toByte),
    cdbvSet ++ Array(tokenAIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(tokenBIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(tokenLiquidityIdStateVar.index, 2.toByte),
    cdbvSet ++ Array(minimumLiquidityStateVar.index, 3.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    cdbvSet ++ Array(swapStatusStateVar.index, 5.toByte)
  )

  val nonReturnType: Array[Byte] = Array[Byte]()
  val onInitTriggerType: Byte = 0

  lazy val wrongDataTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initWrongDataType, initTriggerOpcs))
  lazy val wrongOpcTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initWrongTriggerOpcs))
}
