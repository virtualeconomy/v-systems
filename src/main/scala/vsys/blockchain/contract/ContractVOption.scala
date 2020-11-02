package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractVOption {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger), // Triggers
    Seq(), // Functions
    stateVarSeq, // StateVars
    stateMapSeq, // StateMaps
    Seq()  // Textual
  ).explicitGet()

  // State Var
  val stateVarName = List("maker", "baseTokenId", "targetTokenId", "optionTokenId", "proofTokenId", "executeTime",
                          "executeDeadline", "optionStatus", "maxIssueNum", "reservedOption", "reservedProof",
                          "price", "priceUnit", "tokenLocked", "tokenCollected")
  val makerStateVar: StateVar               = StateVar(0.toByte, DataType.Address.id.toByte)
  val baseTokenIdStateVar: StateVar         = StateVar(1.toByte, DataType.TokenId.id.toByte)
  val targetTokenIdStateVar: StateVar       = StateVar(2.toByte, DataType.TokenId.id.toByte)
  val optionTokenIdStateVar: StateVar       = StateVar(3.toByte, DataType.TokenId.id.toByte)
  val proofTokenIdStateVar: StateVar        = StateVar(4.toByte, DataType.TokenId.id.toByte)
  val executeTimeStateVar: StateVar         = StateVar(5.toByte, DataType.Timestamp.id.toByte)
  val executeDeadlineStateVar: StateVar     = StateVar(6.toByte, DataType.Timestamp.id.toByte)
  val optionStatusStateVar: StateVar        = StateVar(7.toByte, DataType.Boolean.id.toByte)
  val maxIssueNumStateVar: StateVar         = StateVar(8.toByte, DataType.Amount.id.toByte)
  val reservedOptionStateVar: StateVar      = StateVar(9.toByte, DataType.Amount.id.toByte)
  val reservedProofStateVar: StateVar       = StateVar(10.toByte, DataType.Amount.id.toByte)
  val priceStateVar: StateVar               = StateVar(11.toByte, DataType.Amount.id.toByte)
  val priceUnitStateVar: StateVar           = StateVar(12.toByte, DataType.Amount.id.toByte)
  val tokenLockedStateVar: StateVar         = StateVar(13.toByte, DataType.Amount.id.toByte)
  val tokenCollectedStateVar: StateVar      = StateVar(14.toByte, DataType.Amount.id.toByte)
  lazy val stateVarSeq = Seq(makerStateVar.arr, baseTokenIdStateVar.arr, targetTokenIdStateVar.arr,
                             optionTokenIdStateVar.arr, proofTokenIdStateVar.arr,
                             executeTimeStateVar.arr, executeDeadlineStateVar.arr, optionStatusStateVar.arr,
                             maxIssueNumStateVar.arr, reservedOptionStateVar.arr, reservedProofStateVar.arr,
                             priceStateVar.arr, priceUnitStateVar.arr, tokenLockedStateVar.arr, tokenCollectedStateVar.arr)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapBaseTokenBalance         = List("baseTokenBalance", "userAddress", "balance")
  val stateMapTargetTokenBalance       = List("targetTokenBalance", "userAddress", "balance")
  val stateMapOptionTokenBalance       = List("optionTokenBalance", "userAddress", "balance")
  val stateMapProofTokenBalance        = List("proofTokenBalance", "userAddress", "balance")
  val baseTokenBalanceMap: StateMap    = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val targetTokenBalanceMap: StateMap  = StateMap(1.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val optionTokenBalanceMap: StateMap  = StateMap(2.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val proofTokenBalanceMap: StateMap   = StateMap(3.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)

  lazy val stateMapSeq = Seq(baseTokenBalanceMap.arr, targetTokenBalanceMap.arr, optionTokenBalanceMap.arr, proofTokenBalanceMap.arr)
  lazy val stateMapTextual: Array[Byte] = textualStateMap(Seq(stateMapBaseTokenBalance, stateMapTargetTokenBalance,
                                                              stateMapOptionTokenBalance, stateMapProofTokenBalance))

  // Initialization Trigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq("baseTokenId", "targetTokenId", "optionTokenId", "proofTokenId", "executeTime", "executeDeadline") ++
                              Seq("signer", "optionStatus")
  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte, DataType.TokenId.id.toByte,DataType.TokenId.id.toByte,
                                        DataType.TokenId.id.toByte, DataType.Timestamp.id.toByte, DataType.Timestamp.id.toByte)
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
  lazy val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initTriggerOpcs)
  val initTextualBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Deposit Trigger
  val depositId: Short = 1
  val depositPara: Seq[String] = Seq("depositor", "amount", "tokenId") ++
                                 Seq("baseTokenId", "targetTokenId", "optionTokenId", "proofTokenId", "isValidTokenId",
                                     "isBaseToken", "baseTokenIfBlock", "isTargetToken", "targetTokenIfBlock",
                                     "isOptionToken", "optionTokenIfBlock", "isProofToken", "proofTokenIfBlock")
  val depositDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(baseTokenIdStateVar.index, 3.toByte),
    cdbvrGet ++ Array(targetTokenIdStateVar.index, 4.toByte),
    cdbvrGet ++ Array(optionTokenIdStateVar.index, 5.toByte),
    cdbvrGet ++ Array(proofTokenIdStateVar.index, 6.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
    compareBytesEqual ++ Array(2.toByte, 3.toByte, 8.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValAdd ++ Array(baseTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(9.toByte),
    compareBytesEqual ++ Array(2.toByte, 4.toByte, 10.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(11.toByte),
    compareBytesEqual ++ Array(2.toByte, 5.toByte, 12.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValAdd ++ Array(optionTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(13.toByte),
    compareBytesEqual ++ Array(2.toByte, 6.toByte, 14.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValAdd ++ Array(optionTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(15.toByte),
    conditionIf ++ Array(8.toByte, 9.toByte),
    conditionIf ++ Array(10.toByte, 11.toByte),
    conditionIf ++ Array(12.toByte, 13.toByte),
    conditionIf ++ Array(14.toByte, 15.toByte),
    assertTrue ++ Array(7.toByte)
  )
  lazy val depositTrigger: Array[Byte] = getFunctionBytes(depositId, onDepositTriggerType, nonReturnType, depositDataType, depositTriggerOpcs)
  val depositTextualBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  // WithDraw Trigger
  val withdrawId: Short = 2
  val withdrawPara: Seq[String] = Seq("withdrawer", "amount", "tokenId") ++
                                  Seq("baseTokenId", "targetTokenId", "optionTokenId", "proofTokenId", "isValidTokenId",
                                      "isBaseToken", "baseTokenIfBlock", "isTargetToken", "targetTokenIfBlock",
                                      "isOptionToken", "optionTokenIfBlock", "isProofToken", "proofTokenIfBlock")
  val withdrawDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val withdrawTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(baseTokenIdStateVar.index, 3.toByte),
    cdbvrGet ++ Array(targetTokenIdStateVar.index, 4.toByte),
    cdbvrGet ++ Array(optionTokenIdStateVar.index, 5.toByte),
    cdbvrGet ++ Array(proofTokenIdStateVar.index, 6.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
    compareBytesEqual ++ Array(2.toByte, 3.toByte, 8.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValMinus ++ Array(baseTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(9.toByte),
    compareBytesEqual ++ Array(2.toByte, 4.toByte, 10.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValMinus ++ Array(targetTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(11.toByte),
    compareBytesEqual ++ Array(2.toByte, 5.toByte, 12.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValMinus ++ Array(optionTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(13.toByte),
    compareBytesEqual ++ Array(2.toByte, 6.toByte, 14.toByte),
    basicConstantGet ++ DataEntry(genFunctionOpcs(
      Seq(
        cdbvMapValMinus ++ Array(optionTokenBalanceMap.index, 0.toByte, 1.toByte),
        basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
      )
    ), DataType.OpcBlock).bytes ++ Array(15.toByte),
    conditionIf ++ Array(8.toByte, 9.toByte),
    conditionIf ++ Array(10.toByte, 11.toByte),
    conditionIf ++ Array(12.toByte, 13.toByte),
    conditionIf ++ Array(14.toByte, 15.toByte),
    assertTrue ++ Array(7.toByte)
  )
  lazy val withdrawTrigger: Array[Byte] = getFunctionBytes(withdrawId, onWithDrawTriggerType, nonReturnType, withdrawDataType, withdrawTriggerOpcs)
  val withdrawTextualBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  // Functions

  // Textual
}