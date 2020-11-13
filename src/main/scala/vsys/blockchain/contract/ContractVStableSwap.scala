package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVStableSwap {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(), // Triggers
    Seq(), // Functions
    stateVarSeq, // StateVars
    stateMapSeq,  // StateMaps
    Seq()  // Textual
  ).explicitGet()

  // State Var
  val stateVarName = List("maker", "baseTokenId", "targetTokenId", "minSwapAmount", "maxSwapAmount",
                          "lastUpdateTime", "updateGap", "feeBase", "feeTarget")
  val makerStateVar: StateVar               = StateVar(0.toByte, DataType.Address.id.toByte)
  val baseTokenIdStateVar: StateVar         = StateVar(1.toByte, DataType.TokenId.id.toByte)
  val targetTokenIdStateVar: StateVar       = StateVar(2.toByte, DataType.TokenId.id.toByte)
  val minSwapAmountStateVar: StateVar       = StateVar(3.toByte, DataType.Amount.id.toByte)
  val maxSwapAmountStateVar: StateVar       = StateVar(4.toByte, DataType.Amount.id.toByte)
  val lastUpdateTimeStateVar: StateVar      = StateVar(5.toByte, DataType.Timestamp.id.toByte)
  val updateGapStateVar: StateVar           = StateVar(6.toByte, DataType.Timestamp.id.toByte)
  val feeBaseStateVar: StateVar             = StateVar(7.toByte, DataType.Amount.id.toByte)
  val feeTargetStateVar: StateVar           = StateVar(8.toByte, DataType.Amount.id.toByte)
  lazy val stateVarSeq = Seq(makerStateVar.arr, baseTokenIdStateVar.arr, targetTokenIdStateVar.arr,
                             minSwapAmountStateVar.arr, maxSwapAmountStateVar.arr,
                             lastUpdateTimeStateVar.arr, updateGapStateVar.arr,
                             feeBaseStateVar.arr, feeTargetStateVar.arr)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapBaseTokenBalance         = List("baseTokenBalance", "userAddress", "balance")
  val stateMapTargetTokenBalance       = List("targetTokenBalance", "userAddress", "balance")
  val baseTokenBalanceMap: StateMap    = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val targetTokenBalanceMap: StateMap  = StateMap(1.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)

  lazy val stateMapSeq = Seq(baseTokenBalanceMap.arr, targetTokenBalanceMap.arr)
  lazy val stateMapTextual: Array[Byte] = textualStateMap(Seq(stateMapBaseTokenBalance, stateMapTargetTokenBalance))

  // Initialization Trigger

  // Deposit Trigger

  // Withdraw Trigger

  // Functions

  // Textual

}