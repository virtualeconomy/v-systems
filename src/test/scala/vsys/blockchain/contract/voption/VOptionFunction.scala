package vsys.blockchain.contract.voption

import vsys.blockchain.contract.ContractGen.{basicConstantGet, cdbvSet, getFunctionBytes, loadSigner}
import vsys.blockchain.contract.ContractVOption.{baseTokenIdStateVar, executeDeadlineStateVar, executeTimeStateVar, makerStateVar, optionStatusStateVar, optionTokenIdStateVar, proofTokenIdStateVar, targetTokenIdStateVar}
import vsys.blockchain.contract.ContractVSwap._
import vsys.blockchain.contract.{DataEntry, DataType}

trait VOptionFunction {

  val initId: Short = 0

  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte, DataType.TokenId.id.toByte,DataType.TokenId.id.toByte, DataType.TokenId.id.toByte, DataType.Timestamp.id.toByte, DataType.Timestamp.id.toByte)
  val initWrongDataType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.TokenId.id.toByte,DataType.TokenId.id.toByte, DataType.TokenId.id.toByte, DataType.Timestamp.id.toByte, DataType.Timestamp.id.toByte)

  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(6.toByte),
    cdbvSet ++ Array(makerStateVar.index, 6.toByte),
    cdbvSet ++ Array(baseTokenIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(targetTokenIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(optionTokenIdStateVar.index, 2.toByte),
    cdbvSet ++ Array(proofTokenIdStateVar.index, 3.toByte),
    cdbvSet ++ Array(executeTimeStateVar.index, 4.toByte),
    cdbvSet ++ Array(executeDeadlineStateVar.index, 5.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    cdbvSet ++ Array(optionStatusStateVar.index, 5.toByte)
  )

  val initWrongTriggerOpcs: Seq[Array[Byte]] = Seq(
    Array(5.toByte, 3.toByte) ++ loadSigner ++ Array(6.toByte),
    cdbvSet ++ Array(makerStateVar.index, 6.toByte),
    cdbvSet ++ Array(baseTokenIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(targetTokenIdStateVar.index, 1.toByte),
    cdbvSet ++ Array(optionTokenIdStateVar.index, 2.toByte),
    cdbvSet ++ Array(proofTokenIdStateVar.index, 3.toByte),
    cdbvSet ++ Array(executeTimeStateVar.index, 4.toByte),
    cdbvSet ++ Array(executeDeadlineStateVar.index, 5.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    cdbvSet ++ Array(optionStatusStateVar.index, 5.toByte)
  )

  val nonReturnType: Array[Byte] = Array[Byte]()
  val onInitTriggerType: Byte = 0

  lazy val wrongDataTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initWrongDataType, initTriggerOpcs))
  lazy val wrongOpcTrigger: Seq[Array[Byte]] = Seq(getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initWrongTriggerOpcs))
}
