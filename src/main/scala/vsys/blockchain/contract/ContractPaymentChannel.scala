package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.utils.serialization.Deser

object ContractPaymentChannel {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger),
    Seq(createFunc, updateExpiredTimeFunc, chargeFunc, terminateFunc, executeWithdrawFunc, executePaymentFunc),
    Seq(makerStateVar.arr, tokenIdStateVar.arr),
    Seq(balanceMap.arr, channelCreatorMap.arr, channelCreatorPublicKeyMap.arr, channelRecipientMap.arr,
        channelCapacityMap.arr, channelExecutedMap.arr, channelExpiredTimeMap.arr, channelStatusMap.arr),
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
  ).right.get

  // State Var
  val stateVarName = List("maker", "tokenId")
  val makerStateVar: StateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val tokenIdStateVar: StateVar = StateVar(1.toByte, DataType.TokenId.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapName = List("balance", "channelCreator", "channelCreatorPublicKey", "channelRecipient", "channelCapacity", "channelExecuted", "channelExpiredTime", "channelStatus")
  val balanceMap: StateMap                   = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val channelCreatorMap: StateMap            = StateMap(1.toByte, DataType.ShortText.id.toByte, DataType.Address.id.toByte)
  val channelCreatorPublicKeyMap: StateMap   = StateMap(2.toByte, DataType.ShortText.id.toByte, DataType.PublicKey.id.toByte)
  val channelRecipientMap: StateMap          = StateMap(3.toByte, DataType.ShortText.id.toByte, DataType.Address.id.toByte)
  val channelCapacityMap: StateMap           = StateMap(4.toByte, DataType.ShortText.id.toByte, DataType.Amount.id.toByte)
  val channelExecutedMap: StateMap           = StateMap(5.toByte, DataType.ShortText.id.toByte, DataType.Amount.id.toByte)
  val channelExpiredTimeMap: StateMap        = StateMap(6.toByte, DataType.ShortText.id.toByte, DataType.Timestamp.id.toByte)
  val channelStatusMap: StateMap             = StateMap(7.toByte, DataType.ShortText.id.toByte, DataType.Boolean.id.toByte)
  lazy val stateMapTextual: Array[Byte] = Deser.serializeArrays(stateMapName.map(x => Deser.serilizeString(x)))

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

  // Create Function
  val createId: Short = 0
  val createPara: Seq[String] = Seq("recipient", "amount", "expiredTime",
                                    "caller", "callerPublicKey", "channelId", "valueZero", "valueTrue")
  val createDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.Timestamp.id.toByte)
  val createFunctionOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(3.toByte),
    loadPublicKey ++ Array(4.toByte),
    loadTransactionId ++ Array(5.toByte),
    cdbvrConstantGet ++ DataEntry(Longs.toByteArray(0), DataType.Amount).bytes ++ Array(6.toByte),
    cdbvMapValMinus ++ Array(balanceMap.index, 3.toByte, 1.toByte),
    cdbvMapSet ++ Array(channelCreatorMap.index, 5.toByte, 3.toByte),
    cdbvMapSet ++ Array(channelCreatorPublicKeyMap.index, 5.toByte, 4.toByte),
    cdbvMapSet ++ Array(channelRecipientMap.index, 5.toByte, 0.toByte),
    cdbvMapValAdd ++ Array(channelCapacityMap.index, 5.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(channelExecutedMap.index, 5.toByte, 6.toByte),
    cdbvMapSet ++ Array(channelExpiredTimeMap.index, 5.toByte, 2.toByte),
    cdbvrConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
    cdbvMapSet ++ Array(channelStatusMap.index, 5.toByte, 7.toByte)
  )
  lazy val createFunc: Array[Byte] = getFunctionBytes(createId, publicFuncType, nonReturnType, createDataType, createFunctionOpcs)
  val createTextualBytes: Array[Byte] = textualFunc("create", Seq(), createPara)

  // Update Expired Time Function
  val updateExpiredTimeId: Short = 1
  val updateExpiredTimePara: Seq[String] = Seq("channelId", "expiredTime",
                                               "sender", "status", "oldExpiredTime", "res")
  val updateExpiredTimeDataType: Array[Byte] = Array(DataType.ShortText.id.toByte, DataType.Timestamp.id.toByte)
  val updateExpiredTimeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 2.toByte),
    assertCaller ++ Array(2.toByte),
    cdbvrMapGet ++ Array(channelStatusMap.index, 0.toByte, 3.toByte),
    assertTrue ++ Array(3.toByte),
    cdbvrMapGet ++ Array(channelExpiredTimeMap.index, 0.toByte, 4.toByte),
    compareGreater ++ Array(1.toByte, 4.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvMapSet ++ Array(channelExpiredTimeMap.index, 0.toByte, 1.toByte)
  )
  lazy val updateExpiredTimeFunc: Array[Byte] = getFunctionBytes(updateExpiredTimeId, publicFuncType, nonReturnType,
                                                                 updateExpiredTimeDataType, updateExpiredTimeFunctionOpcs)
  val updateExpiredTimeTextualBytes: Array[Byte] = textualFunc("updateExpiredTime", Seq(), updateExpiredTimePara)

  // Charge Function
  val chargeId: Short = 2
  val chargePara: Seq[String] = Seq("channelId", "amount",
                                    "sender")
  val chargeDataType: Array[Byte] = Array(DataType.ShortText.id.toByte, DataType.Amount.id.toByte)
  val chargeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 2.toByte),
    assertCaller ++ Array(2.toByte),
    cdbvMapValMinus ++ Array(balanceMap.index, 2.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(channelCapacityMap.index, 0.toByte, 1.toByte)
  )
  lazy val chargeFunc: Array[Byte] = getFunctionBytes(chargeId, publicFuncType, nonReturnType, chargeDataType, chargeFunctionOpcs)
  val chargeTextualBytes: Array[Byte] = textualFunc("charge", Seq(), chargePara)

  // Terminate Function
  val terminateId: Short = 3
  val terminatePara: Seq[String] = Seq("channelId",
                                       "sender", "currentTime", "gap", "time", "expiredTime", "terminateTime", "valueFalse")
  val terminateDataType: Array[Byte] = Array(DataType.ShortText.id.toByte)
  val terminateFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    loadTimestamp ++ Array(2.toByte),
    cdbvrConstantGet ++ DataEntry(Longs.toByteArray(48 * 3600 * 1000000000L), DataType.Timestamp).bytes ++ Array(3.toByte),
    basicAdd ++ Array(2.toByte, 3.toByte, 4.toByte),
    cdbvrMapGet ++ Array(channelExpiredTimeMap.index, 0.toByte, 5.toByte),
    basicMin ++ Array(4.toByte, 5.toByte, 6.toByte),
    cdbvMapSet ++ Array(channelExpiredTimeMap.index, 0.toByte, 6.toByte),
    cdbvrConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
    cdbvMapSet ++ Array(channelStatusMap.index, 0.toByte, 7.toByte)
  )
  lazy val terminateFunc: Array[Byte] = getFunctionBytes(terminateId, publicFuncType, nonReturnType, terminateDataType, terminateFunctionOpcs)
  val terminateTextualBytes: Array[Byte] = textualFunc("terminate", Seq(), terminatePara)

  // Execute Withdraw Function
  val executeWithdrawId: Short = 4
  val executeWithdrawPara: Seq[String] = Seq("channelId",
                                             "sender", "currentTime", "expiredTime", "res", "currentTotal", "currentExecuted", "toWithdraw")
  val executeWithdrawDataType: Array[Byte] = Array(DataType.ShortText.id.toByte)
  val executeWithdrawFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    loadTimestamp ++ Array(2.toByte),
    cdbvrMapGet ++ Array(channelExpiredTimeMap.index, 0.toByte, 3.toByte),
    compareGreater ++ Array(2.toByte, 3.toByte, 4.toByte),
    assertTrue ++ Array(4.toByte),
    cdbvrMapGetOrDefault ++ Array(channelCapacityMap.index, 0.toByte, 5.toByte),
    cdbvrMapGetOrDefault ++ Array(channelExecutedMap.index, 0.toByte, 6.toByte),
    basicMinus ++ Array(5.toByte, 6.toByte, 7.toByte),
    cdbvMapValAdd ++ Array(channelExecutedMap.index, 0.toByte, 7.toByte),
    cdbvMapValAdd ++ Array(balanceMap.index, 1.toByte, 7.toByte)
  )
  lazy val executeWithdrawFunc: Array[Byte] = getFunctionBytes(executeWithdrawId, publicFuncType, nonReturnType,
                                                               executeWithdrawDataType, executeWithdrawFunctionOpcs)
  val executeWithdrawTextualBytes: Array[Byte] = textualFunc("executeWithdraw", Seq(), executeWithdrawPara)

  // Execute Payment Function
  val executePaymentId: Short = 5
  val executePaymentPara: Seq[String] = Seq("channelId", "amount", "signature",
                                             "recipient", "currentTime", "expiredTime", "res", "senderPublicKey", "toSign",
                                             "currentExecuted", "resPayment", "currentTotal", "resCapacity", "toExecute")
  val executePaymentDataType: Array[Byte] = Array(DataType.ShortText.id.toByte, DataType.Amount.id.toByte, DataType.ShortText.id.toByte)
  val executePaymentFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelRecipientMap.index, 0.toByte, 3.toByte),
    assertCaller ++ Array(3.toByte),
    loadTimestamp ++ Array(4.toByte),
    cdbvrMapGet ++ Array(channelExpiredTimeMap.index, 0.toByte, 5.toByte),
    compareGreater ++ Array(5.toByte, 4.toByte, 6.toByte),
    assertTrue ++ Array(6.toByte),
    cdbvrMapGet ++ Array(channelCreatorPublicKeyMap.index, 0.toByte, 7.toByte),
    basicConcat ++ Array(0.toByte, 1.toByte, 8.toByte),
    assertSig ++ Array(8.toByte, 2.toByte, 7.toByte),
    cdbvrMapGetOrDefault ++ Array(channelExecutedMap.index, 0.toByte, 9.toByte),
    compareGreater ++ Array(1.toByte, 9.toByte, 10.toByte),
    assertTrue ++ Array(10.toByte),
    cdbvrMapGetOrDefault ++ Array(channelCapacityMap.index, 0.toByte, 11.toByte),
    compareGreater ++ Array(11.toByte, 1.toByte, 12.toByte),
    assertTrue ++ Array(12.toByte),
    basicMinus ++ Array(1.toByte, 9.toByte, 13.toByte),
    cdbvMapValAdd ++ Array(channelExecutedMap.index, 0.toByte, 13.toByte),
    cdbvMapValAdd ++ Array(balanceMap.index, 3.toByte, 13.toByte)
  )
  lazy val executePaymentFunc: Array[Byte] = getFunctionBytes(executePaymentId, publicFuncType, nonReturnType,
                                                              executePaymentDataType, executePaymentFunctionOpcs)
  val executePaymentTextualBytes: Array[Byte] = textualFunc("executePayment", Seq(), executePaymentPara)

  // Gen Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(createTextualBytes, updateExpiredTimeTextualBytes, chargeTextualBytes,
                                                                      terminateTextualBytes, executeWithdrawTextualBytes, executePaymentTextualBytes))

}