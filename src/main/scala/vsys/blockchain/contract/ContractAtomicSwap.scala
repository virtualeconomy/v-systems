package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.utils.serialization.Deser

object ContractAtomicSwap {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger),
    Seq(lockFunc, solvePuzzleFunc, expireWithdrawFunc),
    Seq(makerStateVar.arr, tokenIdStateVar.arr),
    Seq(balanceMap.arr, swapOwnerMap.arr, swapRecipientMap.arr,
        swapPuzzleMap.arr, swapAmountMap.arr, swapExpiredTimeMap.arr, swapStatusMap.arr),
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
  ).right.get

  // State Var
  val stateVarName = List("maker", "tokenId")
  val makerStateVar: StateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val tokenIdStateVar: StateVar = StateVar(1.toByte, DataType.TokenId.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapBalance = List("contractBalance", "userAddress", "balance")
  val stateMapOwner = List("swapOwner", "swapId", "owner")
  val stateMapRecipient = List("swapRecipient", "swapId", "recipient")
  val stateMapPuzzle = List("swapPuzzle", "swapId", "puzzle")
  val stateMapAmount = List("swapAmount", "swapId", "amount")
  val stateMapExpiredTime = List("swapExpiredTime", "swapId", "expiredTime")
  val stateMapStatus = List("swapStatus", "swapId", "status")
  val balanceMap: StateMap                = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val swapOwnerMap: StateMap              = StateMap(1.toByte, DataType.ShortText.id.toByte, DataType.Address.id.toByte)
  val swapRecipientMap: StateMap          = StateMap(2.toByte, DataType.ShortText.id.toByte, DataType.Address.id.toByte)
  val swapPuzzleMap: StateMap             = StateMap(3.toByte, DataType.ShortText.id.toByte, DataType.ShortText.id.toByte)
  val swapAmountMap: StateMap             = StateMap(4.toByte, DataType.ShortText.id.toByte, DataType.Amount.id.toByte)
  val swapExpiredTimeMap: StateMap        = StateMap(5.toByte, DataType.ShortText.id.toByte, DataType.Timestamp.id.toByte)
  val swapStatusMap: StateMap             = StateMap(6.toByte, DataType.ShortText.id.toByte, DataType.Boolean.id.toByte)
  lazy val stateMapTextual: Array[Byte] = textualStateMap(Seq(stateMapBalance, stateMapOwner, stateMapRecipient, stateMapPuzzle,
                                                              stateMapAmount, stateMapExpiredTime, stateMapStatus))

  // Initialization Trigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq("tokenId",
                                  "signer")
  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte)
  val initTriggerOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(1.toByte),
    cdbvSet ++ Array(makerStateVar.index, 1.toByte),
    cdbvSet ++ Array(tokenIdStateVar.index, 0.toByte)
  )
  lazy val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initTriggerOpcs)
  val initTextualBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Deposit Trigger
  val depositId: Short = 1
  val depositPara: Seq[String] = Seq("depositor", "amount", "tokenId",
                                     "contractTokenId")
  val depositDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(tokenIdStateVar.index, 3.toByte),
    assertEqual ++ Array(2.toByte, 3.toByte),
    cdbvMapValAdd ++ Array(balanceMap.index, 0.toByte, 1.toByte)
  )
  lazy val depositTrigger: Array[Byte] = getFunctionBytes(depositId, onDepositTriggerType, nonReturnType, depositDataType, depositTriggerOpcs)
  val depositTextualBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  // WithDraw Trigger
  val withdrawId: Short = 2
  val withdrawPara: Seq[String] = Seq("withdrawer", "amount", "tokenId",
                                      "contractTokenId")
  val withdrawDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val withdrawTriggerOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrGet ++ Array(tokenIdStateVar.index, 3.toByte),
    assertEqual ++ Array(2.toByte, 3.toByte),
    cdbvMapValMinus ++ Array(balanceMap.index, 0.toByte, 1.toByte)
  )
  lazy val withdrawTrigger: Array[Byte] = getFunctionBytes(withdrawId, onWithDrawTriggerType, nonReturnType, withdrawDataType, withdrawTriggerOpcs)
  val withdrawTextualBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  // Lock Function
  val lockId: Short = 0
  val lockPara: Seq[String] = Seq("amount", "recipient", "puzzle", "expiredTime",
                                  "caller", "txId", "valueTrue")
  val lockDataType: Array[Byte] = Array(DataType.Amount.id.toByte, DataType.Address.id.toByte, DataType.ShortText.id.toByte, DataType.Timestamp.id.toByte)
  val lockFunctionOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(4.toByte),
    cdbvMapValMinus ++ Array(balanceMap.index, 4.toByte, 0.toByte),
    loadTransactionId ++ Array(5.toByte),
    cdbvMapSet ++ Array(swapOwnerMap.index, 5.toByte, 4.toByte),
    cdbvMapSet ++ Array(swapRecipientMap.index, 5.toByte, 1.toByte),
    cdbvMapSet ++ Array(swapPuzzleMap.index, 5.toByte, 2.toByte),
    cdbvMapSet ++ Array(swapAmountMap.index, 5.toByte, 0.toByte),
    cdbvMapSet ++ Array(swapExpiredTimeMap.index, 5.toByte, 3.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(6.toByte),
    cdbvMapSet ++ Array(swapStatusMap.index, 5.toByte, 6.toByte)
  )
  lazy val lockFunc: Array[Byte] = getFunctionBytes(lockId, publicFuncType, nonReturnType, lockDataType, lockFunctionOpcs)
  val lockTextualBytes: Array[Byte] = textualFunc("lock", Seq(), lockPara)

  // Solve Puzzle Function
  val solvePuzzleId: Short = 1
  val solvePuzzlePara: Seq[String] = Seq("txId", "key",
                                         "status", "recipient", "currentTime", "expiredTime", "res", "puzzle", "amount", "valueFalse")
  val solvePuzzleDataType: Array[Byte] = Array(DataType.ShortText.id.toByte, DataType.ShortText.id.toByte)
  val solvePuzzleFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(swapStatusMap.index, 0.toByte, 2.toByte),
    assertTrue ++ Array(2.toByte),
    cdbvrMapGet ++ Array(swapRecipientMap.index, 0.toByte, 3.toByte),
    assertCaller ++ Array(3.toByte),
    loadTimestamp ++ Array(4.toByte),
    cdbvrMapGet ++ Array(swapExpiredTimeMap.index, 0.toByte, 5.toByte),
    compareGreater ++ Array(5.toByte, 4.toByte, 6.toByte),
    assertTrue ++ Array(6.toByte),
    cdbvrMapGet ++ Array(swapPuzzleMap.index, 0.toByte, 7.toByte),
    assertHash ++ Array(7.toByte, 1.toByte),
    cdbvrMapGet ++ Array(swapAmountMap.index, 0.toByte, 8.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(9.toByte),
    cdbvMapSet ++ Array(swapStatusMap.index, 0.toByte, 9.toByte),
    cdbvMapValAdd ++ Array(balanceMap.index, 3.toByte, 8.toByte)
  )
  lazy val solvePuzzleFunc: Array[Byte] = getFunctionBytes(solvePuzzleId, publicFuncType, nonReturnType, solvePuzzleDataType, solvePuzzleFunctionOpcs)
  val solvePuzzleTextualBytes: Array[Byte] = textualFunc("solvePuzzle", Seq(), solvePuzzlePara)

  // Expire Withdraw Function
  val expireWithdrawId: Short = 2
  val expireWithdrawPara: Seq[String] = Seq("txId",
                                            "status", "owner", "currentTime", "expiredTime", "res", "amount", "valueFalse")
  val expireWithdrawDataType: Array[Byte] = Array(DataType.ShortText.id.toByte)
  val expireWithdrawFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(swapStatusMap.index, 0.toByte, 1.toByte),
    assertTrue ++ Array(1.toByte),
    cdbvrMapGet ++ Array(swapOwnerMap.index, 0.toByte, 2.toByte),
    assertCaller ++ Array(2.toByte),
    loadTimestamp ++ Array(3.toByte),
    cdbvrMapGet ++ Array(swapExpiredTimeMap.index, 0.toByte, 4.toByte),
    compareGreater ++ Array(3.toByte, 4.toByte, 5.toByte),
    assertTrue  ++ Array(5.toByte),
    cdbvrMapGet ++ Array(swapAmountMap.index, 0.toByte, 6.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
    cdbvMapSet ++ Array(swapStatusMap.index, 0.toByte, 7.toByte),
    cdbvMapValAdd ++ Array(balanceMap.index, 2.toByte, 6.toByte)
  )
  lazy val expireWithdrawFunc: Array[Byte] = getFunctionBytes(expireWithdrawId, publicFuncType, nonReturnType, expireWithdrawDataType, expireWithdrawFunctionOpcs)
  val expireWithdrawTextualBytes: Array[Byte] = textualFunc("expireWithdraw", Seq(), expireWithdrawPara)

  // Gen Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(lockTextualBytes, solvePuzzleTextualBytes, expireWithdrawTextualBytes))

}