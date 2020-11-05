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
  // Supersede
  val supersedeId: Short = 0
  val supersedePara: Seq[String] = Seq("newOwner",
                                       "maker")
  val supersedeDataType: Array[Byte] = Array(DataType.Account.id.toByte)
  val supersedeOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrGet ++ Array(makerStateVar.index, 1.toByte),
    assertSigner ++ Array(1.toByte),
    cdbvSet ++ Array(makerStateVar.index, 0.toByte))
  lazy val supersedeFunc: Array[Byte] = getFunctionBytes(supersedeId, publicFuncType, nonReturnType, supersedeDataType, supersedeOpcs)
  val supersedeTextualBytes: Array[Byte] = textualFunc("supersede", Seq(), supersedePara)

  // Activate Option
  val activateId: Short = 1
  val activatePara: Seq[String] = Seq("maxIssueNum", "price", "priceUnit") ++
                                  Seq("maker", "optionStatus", "valueFalse", "amountZero", "isValidIssueNum",
                                      "isValidPrice", "isValidPriceUnit", "optionTokenNum", "proofTokenNum", "valueTrue")
  val activateDataType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.Amount.id.toByte)
  val activateOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(makerStateVar.index, 3.toByte),
    assertCaller ++ Array(3.toByte),
    cdbvrGet ++ Array(optionStatusStateVar.index, 4.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(5.toByte),
    assertEqual ++ Array(4.toByte, 5.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(0L), DataType.Amount).bytes ++ Array(6.toByte),
    compareGreater ++ Array(0.toByte, 6.toByte, 7.toByte),
    assertTrue ++ Array(7.toByte),
    compareGreater ++ Array(1.toByte, 6.toByte, 8.toByte),
    assertTrue ++ Array(8.toByte),
    compareGreater ++ Array(2.toByte, 6.toByte, 9.toByte),
    assertTrue ++ Array(9.toByte),
    cdbvrMapGetOrDefault ++ Array(optionTokenBalanceMap.index, 3.toByte, 10.toByte),
    cdbvrMapGetOrDefault ++ Array(proofTokenBalanceMap.index, 3.toByte, 11.toByte),
    assertEqual ++ Array(10.toByte, 0.toByte),
    assertEqual ++ Array(11.toByte, 0.toByte),
    cdbvMapValMinus ++ Array(optionTokenBalanceMap.index, 3.toByte, 0.toByte),
    cdbvMapValMinus ++ Array(proofTokenBalanceMap.index, 3.toByte, 0.toByte),
    cdbvSet ++ Array(maxIssueNumStateVar.index, 0.toByte),
    cdbvStateValAdd ++ Array(reservedOptionStateVar.index, 0.toByte),
    cdbvStateValAdd ++ Array(reservedProofStateVar.index, 0.toByte),
    cdbvSet ++ Array(priceStateVar.index, 1.toByte),
    cdbvSet ++ Array(priceUnitStateVar.index, 2.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(12.toByte),
    cdbvSet ++ Array(optionStatusStateVar.index, 12.toByte)
  )
  lazy val activateFunc: Array[Byte] = getFunctionBytes(activateId, publicFuncType, nonReturnType, activateDataType, activateOpcs)
  val activateTextualBytes: Array[Byte] = textualFunc("activate", Seq(), activatePara)

  // Common Option Code
  val commonOptionPara: Seq[String] = Seq("amount") ++
                                      Seq("caller", "OptionStatus", "currentTime")
  val commonOptionDataType: Array[Byte] = Array(DataType.Amount.id.toByte)
  val commonOptionOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(1.toByte),
    cdbvrGet ++ Array(optionStatusStateVar.index, 2.toByte),
    assertTrue ++ Array(2.toByte),
    loadTimestamp ++ Array(3.toByte)
  )

  // Mint Option
  val mintId: Short = 2
  val mintPara: Seq[String] = commonOptionPara ++
                              Seq("executeTime", "isValidTime")
  val mintOpcs: Seq[Array[Byte]] = commonOptionOpcs ++ Seq(
    cdbvrGet ++ Array(executeTimeStateVar.index, 4.toByte),
    compareGreater ++ Array(4.toByte, 3.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvMapValMinus ++ Array(targetTokenBalanceMap.index, 1.toByte, 0.toByte),
    cdbvStateValMinus ++ Array(reservedOptionStateVar.index, 0.toByte),
    cdbvStateValMinus ++ Array(reservedProofStateVar.index, 0.toByte),
    cdbvStateValAdd ++ Array(tokenLockedStateVar.index, 0.toByte),
    cdbvMapValAdd ++ Array(optionTokenBalanceMap.index, 1.toByte, 0.toByte),
    cdbvMapValAdd ++ Array(proofTokenBalanceMap.index, 1.toByte, 0.toByte)
  )
  lazy val mintFunc: Array[Byte] = getFunctionBytes( mintId, publicFuncType, nonReturnType, commonOptionDataType, mintOpcs)
  val mintTextualBytes: Array[Byte] = textualFunc("mint", Seq(), mintPara)

  // Unlock Option
  val unlockId: Short = 3
  val unlockPara: Seq[String] = commonOptionPara ++
                                Seq("executeDeadline", "isValidTime")
  val unlockOpcs: Seq[Array[Byte]] = commonOptionOpcs ++ Seq(
    cdbvrGet ++ Array(executeDeadlineStateVar.index, 4.toByte),
    compareGreater ++ Array(4.toByte, 3.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvMapValMinus ++ Array(optionTokenBalanceMap.index, 1.toByte, 0.toByte),
    cdbvMapValMinus ++ Array(proofTokenBalanceMap.index, 1.toByte, 0.toByte),
    cdbvStateValAdd ++ Array(reservedOptionStateVar.index, 0.toByte),
    cdbvStateValAdd ++ Array(reservedProofStateVar.index, 0.toByte),
    cdbvStateValMinus ++ Array(tokenLockedStateVar.index, 0.toByte),
    cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 1.toByte, 0.toByte)
  )
  lazy val unlockFunc: Array[Byte] = getFunctionBytes( unlockId, publicFuncType, nonReturnType, commonOptionDataType, unlockOpcs)
  val unlockTextualBytes: Array[Byte] = textualFunc("unlock", Seq(), unlockPara)

  // Execute Option
  val executeId: Short = 4
  val executePara: Seq[String] = commonOptionPara ++
                                 Seq("executeDeadline", "isBeforeDeadline", "executeTime", "isValidToExecute",
                                     "price", "priceUnit", "bigIntType", "amountBigInt", "priceBigInt", "priceUnitBigInt",
                                     "costWithUnitBigInt", "costBigInt", "amountType", "cost", "amountOne", "fixedCost")
  val executeOpcs: Seq[Array[Byte]] = commonOptionOpcs ++ Seq(
    cdbvrGet ++ Array(executeDeadlineStateVar.index, 4.toByte),
    compareGreater ++ Array(4.toByte, 3.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvrGet ++ Array(executeTimeStateVar.index, 6.toByte),
    compareGreaterEqual ++ Array(3.toByte, 6.toByte, 7.toByte),
    assertTrue ++ Array(7.toByte),
    cdbvMapValMinus ++ Array(optionTokenBalanceMap.index, 1.toByte, 0.toByte),
    cdbvStateValAdd ++ Array(reservedOptionStateVar.index, 0.toByte),
    cdbvrGet ++ Array(priceStateVar.index, 8.toByte),
    cdbvrGet ++ Array(priceUnitStateVar.index, 9.toByte),
    basicConstantGet ++ DataEntry(Array(DataType.BigInteger.id.toByte), DataType.DataTypeObj).bytes ++ Array(10.toByte),
    basicConvert ++ Array(0.toByte, 10.toByte, 11.toByte),
    basicConvert ++ Array(8.toByte, 10.toByte, 12.toByte),
    basicConvert ++ Array(9.toByte, 10.toByte, 13.toByte),
    basicMultiply ++ Array(11.toByte, 12.toByte, 14.toByte),
    basicDivide ++ Array(14.toByte, 13.toByte, 15.toByte),
    basicConstantGet ++ DataEntry(Array(DataType.Amount.id.toByte), DataType.DataTypeObj).bytes ++ Array(16.toByte),
    basicConvert ++ Array(15.toByte, 16.toByte, 17.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(1L), DataType.Amount).bytes ++ Array(18.toByte),
    basicAdd ++ Array(17.toByte, 18.toByte, 19.toByte),
    cdbvMapValMinus ++ Array(baseTokenBalanceMap.index, 1.toByte, 19.toByte),
    cdbvStateValAdd ++ Array(tokenCollectedStateVar.index, 19.toByte),
    cdbvStateValMinus ++ Array(tokenLockedStateVar.index, 0.toByte),
    cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 1.toByte, 0.toByte)
  )
  lazy val executeFunc: Array[Byte] = getFunctionBytes( executeId, publicFuncType, nonReturnType, commonOptionDataType, executeOpcs)
  val executeTextualBytes: Array[Byte] = textualFunc("execute", Seq(), executePara)

  val collectId: Short = 5
  val collectPara: Seq[String] = commonOptionPara ++
                                 Seq("executeDeadline", "isValidTime", "maxIssue", "reservedProof", "isValidCollect", "uncollectedProof",
                                     "bigIntType", "baseNumber", "baseNumberBigInt", "amountBigInt", "uncollectedProofBigInt",
                                     "baseNumerator", "baseCollectBigInt", "amountType", "baseCollect", "targetNumber", "targetNumberBigInt",
                                     "targetNumerator", "targetCollectBigInt", "targetCollect")
  val collectOpcs: Seq[Array[Byte]] = commonOptionOpcs ++ Seq(
    cdbvrGet ++ Array(executeDeadlineStateVar.index, 4.toByte),
    compareGreater ++ Array(3.toByte, 4.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvrGet ++ Array(maxIssueNumStateVar.index, 6.toByte),
    cdbvrGetOrDefault ++ Array(reservedProofStateVar.index, 7.toByte),
    compareGreater ++ Array(6.toByte, 7.toByte, 8.toByte),
    assertTrue ++ Array(8.toByte),
    basicMinus ++ Array(6.toByte, 7.toByte, 9.toByte),
    cdbvMapValMinus ++ Array(proofTokenBalanceMap.index, 1.toByte, 0.toByte),
    cdbvStateValAdd ++ Array(reservedProofStateVar.index, 0.toByte),
    basicConstantGet ++ DataEntry(Array(DataType.BigInteger.id.toByte), DataType.DataTypeObj).bytes ++ Array(10.toByte),
    cdbvrGetOrDefault ++ Array(tokenCollectedStateVar.index, 11.toByte),
    basicConvert ++ Array(11.toByte, 10.toByte, 12.toByte),
    basicConvert ++ Array(0.toByte, 10.toByte, 13.toByte),
    basicConvert ++ Array(9.toByte, 10.toByte, 14.toByte),
    basicMultiply ++ Array(12.toByte, 13.toByte, 15.toByte),
    basicDivide ++ Array(15.toByte, 14.toByte, 16.toByte),
    basicConstantGet ++ DataEntry(Array(DataType.Amount.id.toByte), DataType.DataTypeObj).bytes ++ Array(17.toByte),
    basicConvert ++ Array(16.toByte, 17.toByte, 18.toByte),
    cdbvrGetOrDefault ++ Array(tokenLockedStateVar.index, 19.toByte),
    basicConvert ++ Array(19.toByte, 10.toByte, 20.toByte),
    basicMultiply ++ Array(20.toByte, 13.toByte, 21.toByte),
    basicDivide ++ Array(21.toByte, 14.toByte, 22.toByte),
    basicConvert ++ Array(22.toByte, 17.toByte, 23.toByte),
    cdbvStateValMinus ++ Array(tokenCollectedStateVar.index, 18.toByte),
    cdbvMapValAdd ++ Array(baseTokenBalanceMap.index, 1.toByte, 18.toByte),
    cdbvStateValMinus ++ Array(tokenLockedStateVar.index, 23.toByte),
    cdbvMapValAdd ++ Array(targetTokenBalanceMap.index, 1.toByte, 23.toByte)
  )
  lazy val collectFunc: Array[Byte] = getFunctionBytes( collectId, publicFuncType, nonReturnType, commonOptionDataType, collectOpcs)
  val collectTextualBytes: Array[Byte] = textualFunc("collect", Seq(), collectPara)

  // Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(supersedeTextualBytes, activateTextualBytes, mintTextualBytes,
    unlockTextualBytes, executeTextualBytes, collectTextualBytes))

}