package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractPaymentChannel {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger),
    Seq(createAndLoadFunc, extendExpirationTimeFunc, loadFunc, abortFunc, unloadFunc, collectPaymentFunc),
    Seq(makerStateVar.arr, tokenIdStateVar.arr),
    Seq(balanceMap.arr, channelCreatorMap.arr, channelCreatorPublicKeyMap.arr, channelRecipientMap.arr,
      accumulatedLoadMap.arr, accumulatedPaymentMap.arr, channelExpirationTimeMap.arr, channelStatusMap.arr),
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
  ).explicitGet()

  // State Var
  val stateVarName = List("maker", "tokenId")
  val makerStateVar: StateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val tokenIdStateVar: StateVar = StateVar(1.toByte, DataType.TokenId.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapName = List("balance", "channelCreator", "channelCreatorPublicKey", "channelRecipient", "accumulatedLoad", "accumulatedPayment", "channelExpirationTime", "channelStatus")
  val balanceMap: StateMap                   = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val channelCreatorMap: StateMap            = StateMap(1.toByte, DataType.ShortBytes.id.toByte, DataType.Address.id.toByte)
  val channelCreatorPublicKeyMap: StateMap   = StateMap(2.toByte, DataType.ShortBytes.id.toByte, DataType.PublicKey.id.toByte)
  val channelRecipientMap: StateMap          = StateMap(3.toByte, DataType.ShortBytes.id.toByte, DataType.Address.id.toByte)
  val accumulatedLoadMap: StateMap           = StateMap(4.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val accumulatedPaymentMap: StateMap        = StateMap(5.toByte, DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val channelExpirationTimeMap: StateMap     = StateMap(6.toByte, DataType.ShortBytes.id.toByte, DataType.Timestamp.id.toByte)
  val channelStatusMap: StateMap             = StateMap(7.toByte, DataType.ShortBytes.id.toByte, DataType.Boolean.id.toByte)
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

  // Create And Load Function
  val createAndLoadId: Short = 0
  val createAndLoadPara: Seq[String] = Seq("recipient", "amount", "expirationTime",
                                           "caller", "callerPublicKey", "channelId", "valueZero", "valueTrue")
  val createAndLoadDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.Timestamp.id.toByte)
  val createAndLoadFunctionOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(3.toByte),
    loadPublicKey ++ Array(4.toByte),
    loadTransactionId ++ Array(5.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(0), DataType.Amount).bytes ++ Array(6.toByte),
    cdbvMapValMinus ++ Array(balanceMap.index, 3.toByte, 1.toByte),
    cdbvMapSet ++ Array(channelCreatorMap.index, 5.toByte, 3.toByte),
    cdbvMapSet ++ Array(channelCreatorPublicKeyMap.index, 5.toByte, 4.toByte),
    cdbvMapSet ++ Array(channelRecipientMap.index, 5.toByte, 0.toByte),
    cdbvMapValAdd ++ Array(accumulatedLoadMap.index, 5.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(accumulatedPaymentMap.index, 5.toByte, 6.toByte),
    cdbvMapSet ++ Array(channelExpirationTimeMap.index, 5.toByte, 2.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
    cdbvMapSet ++ Array(channelStatusMap.index, 5.toByte, 7.toByte)
  )
  lazy val createAndLoadFunc: Array[Byte] = getFunctionBytes(createAndLoadId, publicFuncType, nonReturnType,
                                                             createAndLoadDataType, createAndLoadFunctionOpcs)
  val createAndLoadTextualBytes: Array[Byte] = textualFunc("createAndLoad", Seq(), createAndLoadPara)

  // Update Expiration Time Function
  val extendExpirationTimeId: Short = 1
  val extendExpirationTimePara: Seq[String] = Seq("channelId", "expirationTime",
                                                  "sender", "status", "oldExpirationTime", "isValidExtension")
  val extendExpirationTimeDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte, DataType.Timestamp.id.toByte)
  val extendExpirationTimeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 2.toByte),
    assertCaller ++ Array(2.toByte),
    cdbvrMapGet ++ Array(channelStatusMap.index, 0.toByte, 3.toByte),
    assertTrue ++ Array(3.toByte),
    cdbvrMapGet ++ Array(channelExpirationTimeMap.index, 0.toByte, 4.toByte),
    compareGreater ++ Array(1.toByte, 4.toByte, 5.toByte),
    assertTrue ++ Array(5.toByte),
    cdbvMapSet ++ Array(channelExpirationTimeMap.index, 0.toByte, 1.toByte)
  )
  lazy val extendExpirationTimeFunc: Array[Byte] = getFunctionBytes(extendExpirationTimeId, publicFuncType, nonReturnType,
                                                                    extendExpirationTimeDataType, extendExpirationTimeFunctionOpcs)
  val extendExpirationTimeTextualBytes: Array[Byte] = textualFunc("extendExpirationTime", Seq(), extendExpirationTimePara)

  // Load Function
  val loadId: Short = 2
  val loadPara: Seq[String] = Seq("channelId", "amount",
                                  "sender")
  val loadDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte)
  val loadFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 2.toByte),
    assertCaller ++ Array(2.toByte),
    cdbvMapValMinus ++ Array(balanceMap.index, 2.toByte, 1.toByte),
    cdbvMapValAdd ++ Array(accumulatedLoadMap.index, 0.toByte, 1.toByte)
  )
  lazy val loadFunc: Array[Byte] = getFunctionBytes(loadId, publicFuncType, nonReturnType, loadDataType, loadFunctionOpcs)
  val loadTextualBytes: Array[Byte] = textualFunc("load", Seq(), loadPara)

  // Abort Function
  val abortId: Short = 3
  val abortPara: Seq[String] = Seq("channelId",
                                   "sender", "currentTime", "abortDuration", "time", "expirationTime", "abortTime", "valueFalse")
  val abortDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte)
  val abortFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    loadTimestamp ++ Array(2.toByte),
    basicConstantGet ++ DataEntry(Longs.toByteArray(48 * 3600 * 1000000000L), DataType.Timestamp).bytes ++ Array(3.toByte),
    basicAdd ++ Array(2.toByte, 3.toByte, 4.toByte),
    cdbvrMapGet ++ Array(channelExpirationTimeMap.index, 0.toByte, 5.toByte),
    basicMin ++ Array(4.toByte, 5.toByte, 6.toByte),
    cdbvMapSet ++ Array(channelExpirationTimeMap.index, 0.toByte, 6.toByte),
    basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(7.toByte),
    cdbvMapSet ++ Array(channelStatusMap.index, 0.toByte, 7.toByte)
  )
  lazy val abortFunc: Array[Byte] = getFunctionBytes(abortId, publicFuncType, nonReturnType, abortDataType, abortFunctionOpcs)
  val abortTextualBytes: Array[Byte] = textualFunc("abort", Seq(), abortPara)

  // Unload Function
  val unloadId: Short = 4
  val unloadPara: Seq[String] = Seq("channelId",
                                    "sender", "currentTime", "expirationTime", "isExpired", "loadedSoFar", "paidSoFar", "toUnload")
  val unloadDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte)
  val unloadFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 1.toByte),
    assertCaller ++ Array(1.toByte),
    loadTimestamp ++ Array(2.toByte),
    cdbvrMapGet ++ Array(channelExpirationTimeMap.index, 0.toByte, 3.toByte),
    compareGreater ++ Array(2.toByte, 3.toByte, 4.toByte),
    assertTrue ++ Array(4.toByte),
    cdbvrMapGetOrDefault ++ Array(accumulatedLoadMap.index, 0.toByte, 5.toByte),
    cdbvrMapGetOrDefault ++ Array(accumulatedPaymentMap.index, 0.toByte, 6.toByte),
    basicMinus ++ Array(5.toByte, 6.toByte, 7.toByte),
    cdbvMapValAdd ++ Array(accumulatedPaymentMap.index, 0.toByte, 7.toByte),
    cdbvMapValAdd ++ Array(balanceMap.index, 1.toByte, 7.toByte)
  )
  lazy val unloadFunc: Array[Byte] = getFunctionBytes(unloadId, publicFuncType, nonReturnType, unloadDataType, unloadFunctionOpcs)
  val unloadTextualBytes: Array[Byte] = textualFunc("unload", Seq(), unloadPara)

  // Collect Payment Function
  val collectPaymentId: Short = 5
  val collectPaymentPara: Seq[String] = Seq("channelId", "amount", "signature",
                                             "recipient", "currentTime", "expirationTime", "notExpired", "senderPublicKey", "toSign",
                                             "paidSoFar", "monotomicIncreasing", "loadedSoFar", "isValidAmount", "payment")
  val collectPaymentDataType: Array[Byte] = Array(DataType.ShortBytes.id.toByte, DataType.Amount.id.toByte, DataType.ShortBytes.id.toByte)
  val collectPaymentFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelRecipientMap.index, 0.toByte, 3.toByte),
    assertCaller ++ Array(3.toByte),
    loadTimestamp ++ Array(4.toByte),
    cdbvrMapGet ++ Array(channelExpirationTimeMap.index, 0.toByte, 5.toByte),
    compareGreater ++ Array(5.toByte, 4.toByte, 6.toByte),
    assertTrue ++ Array(6.toByte),
    cdbvrMapGet ++ Array(channelCreatorPublicKeyMap.index, 0.toByte, 7.toByte),
    basicConcat ++ Array(0.toByte, 1.toByte, 8.toByte),
    assertSig ++ Array(8.toByte, 2.toByte, 7.toByte),
    cdbvrMapGetOrDefault ++ Array(accumulatedPaymentMap.index, 0.toByte, 9.toByte),
    compareGreater ++ Array(1.toByte, 9.toByte, 10.toByte),
    assertTrue ++ Array(10.toByte),
    cdbvrMapGetOrDefault ++ Array(accumulatedLoadMap.index, 0.toByte, 11.toByte),
    compareGreater ++ Array(11.toByte, 1.toByte, 12.toByte),
    assertTrue ++ Array(12.toByte),
    basicMinus ++ Array(1.toByte, 9.toByte, 13.toByte),
    cdbvMapValAdd ++ Array(accumulatedPaymentMap.index, 0.toByte, 13.toByte),
    cdbvMapValAdd ++ Array(balanceMap.index, 3.toByte, 13.toByte)
  )
  lazy val collectPaymentFunc: Array[Byte] = getFunctionBytes(collectPaymentId, publicFuncType, nonReturnType,
                                                              collectPaymentDataType, collectPaymentFunctionOpcs)
  val collectPaymentTextualBytes: Array[Byte] = textualFunc("collectPayment", Seq(), collectPaymentPara)

  // Gen Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initTextualBytes, depositTextualBytes, withdrawTextualBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(createAndLoadTextualBytes, extendExpirationTimeTextualBytes, loadTextualBytes,
                                                                      abortTextualBytes, unloadTextualBytes, collectPaymentTextualBytes))

}