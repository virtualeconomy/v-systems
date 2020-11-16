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
  val stateVarName = List("maker", "baseTokenId", "targetTokenId", "swapStatus", "minSwapAmount", "maxSwapAmount",
                          "lastUpdateTime", "updateGap", "feeBase", "feeTarget")
  val makerStateVar: StateVar               = StateVar(0.toByte, DataType.Address.id.toByte)
  val baseTokenIdStateVar: StateVar         = StateVar(1.toByte, DataType.TokenId.id.toByte)
  val targetTokenIdStateVar: StateVar       = StateVar(2.toByte, DataType.TokenId.id.toByte)
  val swapStatusStateVar: StateVar          = StateVar(3.toByte, DataType.Boolean.id.toByte)
  val minSwapAmountStateVar: StateVar       = StateVar(4.toByte, DataType.Amount.id.toByte)
  val maxSwapAmountStateVar: StateVar       = StateVar(5.toByte, DataType.Amount.id.toByte)
  val lastUpdateTimeStateVar: StateVar      = StateVar(6.toByte, DataType.Timestamp.id.toByte)
  val updateGapStateVar: StateVar           = StateVar(7.toByte, DataType.Timestamp.id.toByte)
  val feeBaseStateVar: StateVar             = StateVar(8.toByte, DataType.Amount.id.toByte)
  val feeTargetStateVar: StateVar           = StateVar(9.toByte, DataType.Amount.id.toByte)
  lazy val stateVarSeq = Seq(makerStateVar.arr, baseTokenIdStateVar.arr, targetTokenIdStateVar.arr, swapStatusStateVar.arr,
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
  val initId: Short = 0
  val initPara: Seq[String] = Seq("baseTokenId", "targetTokenId") ++
                              Seq("signer", "swapStatus")
  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte, DataType.TokenId.id.toByte)
  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(2.toByte),
    cdbvSet ++ Array(makerStateVar.index, 2.toByte),
    cdbvSet ++ Array(baseTokenIdStateVar.index, 0.toByte),
    cdbvSet ++ Array(targetTokenIdStateVar.index, 1.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    cdbvSet ++ Array(swapStatusStateVar.index, 5.toByte)
  )
  lazy val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initTriggerOpcs)
  val initTextualBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Deposit Trigger
  val depositId: Short = 1
  val depositPara: Seq[String] = Seq("depositor", "amount", "tokenId") ++
                                 Seq("baseTokenId", "targetTokenId", "isValidTokenId",
                                     "isBaseToken", "baseTokenIfBlock", "isTargetToken", "targetTokenIfBlock")
  val depositDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(baseTokenIdStateVar.index, 3.toByte),
    cdbvrGet ++ Array(targetTokenIdStateVar.index, 4.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    compareBytesEqual ++ Array(2.toByte, 3.toByte, 6.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValAdd ++ Array(baseTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(7.toByte),
    compareBytesEqual ++ Array(2.toByte, 4.toByte, 8.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(11.toByte),
    conditionIf ++ Array(6.toByte, 7.toByte),
    conditionIf ++ Array(8.toByte, 10.toByte),
    assertTrue ++ Array(5.toByte)
  )
  lazy val depositTrigger: Array[Byte] = getFunctionBytes(depositId, onDepositTriggerType, nonReturnType, depositDataType, depositTriggerOpcs)
  val depositTextualBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  // Withdraw Trigger

  // Functions

  // Textual

}