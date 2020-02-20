package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.utils.serialization.Deser

object ContractPaymentChannel {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger),
    Seq(createFunc, updateExpiredTimeFunc),
    Seq(makerStateVar.arr, tokenIdStateVar.arr),
    Seq(balanceMap.arr, channelCreatorMap.arr, channelCreatorPublicKeyMap.arr, channelRecipientMap.arr,
        channelCapacityMap.arr, channelExecutedMap.arr, channelExpiredTimeMap.arr),
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
  ).right.get

  // State Var
  val stateVarName = List("maker", "tokenId")
  val makerStateVar: StateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val tokenIdStateVar: StateVar = StateVar(1.toByte, DataType.TokenId.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapName = List("balance", "channelCreator", "channelCreatorPublicKey", "channelRecipient", "channelCapacity", "channelExecuted", "channelExpiredTime")
  val balanceMap: StateMap                   = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val channelCreatorMap: StateMap            = StateMap(1.toByte, DataType.ShortText.id.toByte, DataType.Address.id.toByte)
  val channelCreatorPublicKeyMap: StateMap   = StateMap(2.toByte, DataType.ShortText.id.toByte, DataType.PublicKey.id.toByte)
  val channelRecipientMap: StateMap          = StateMap(2.toByte, DataType.ShortText.id.toByte, DataType.Address.id.toByte)
  val channelCapacityMap: StateMap           = StateMap(3.toByte, DataType.ShortText.id.toByte, DataType.Amount.id.toByte)
  val channelExecutedMap: StateMap           = StateMap(4.toByte, DataType.ShortText.id.toByte, DataType.Amount.id.toByte)
  val channelExpiredTimeMap: StateMap        = StateMap(5.toByte, DataType.ShortText.id.toByte, DataType.Timestamp.id.toByte)
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
  val initFuncBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

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
  val depositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

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
  val withdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  // Create Function
  val createId: Short = 0
  val createPara: Seq[String] = Seq("recipient", "amount", "expiredTime",
                                    "caller", "callerPublicKey", "channelId", "valueZero")
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
    cdbvMapSet ++ Array(channelExpiredTimeMap.index, 5.toByte, 2.toByte)
  )
  lazy val createFunc: Array[Byte] = getFunctionBytes(createId, publicFuncType, nonReturnType, createDataType, createFunctionOpcs)
  val createFuncBytes: Array[Byte] = textualFunc("create", Seq(), createPara)

  // Update Expired Time Function
  val updateExpiredTimeId: Short = 1
  val updateExpiredTimePara: Seq[String] = Seq("channelId", "expiredTime",
                                               "sender", "oldExpiredTime", "res")
  val updateExpiredTimeDataType: Array[Byte] = Array(DataType.ShortText.id.toByte, DataType.Timestamp.id.toByte)
  val updateExpiredTimeFunctionOpcs: Seq[Array[Byte]] = Seq(
    cdbvrMapGet ++ Array(channelCreatorMap.index, 0.toByte, 2.toByte),
    assertCaller ++ Array(2.toByte),
    cdbvrGet ++ Array(channelExpiredTimeMap.index, 0.toByte, 3.toByte),
    compareGreater ++ Array(1.toByte, 3.toByte, 4.toByte),
    assertTrue ++ Array(4.toByte),
    cdbvMapSet ++ Array(channelExpiredTimeMap.index, 0.toByte, 1.toByte)
  )
  lazy val updateExpiredTimeFunc: Array[Byte] = getFunctionBytes(updateExpiredTimeId, publicFuncType, nonReturnType,
                                                                 updateExpiredTimeDataType, updateExpiredTimeFunctionOpcs)
  val updateExpiredTimeFuncBytes: Array[Byte] = textualFunc("updateExpiredTime", Seq(), updateExpiredTimePara)

  // Gen Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes, depositFuncBytes, withdrawFuncBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(createFuncBytes, updateExpiredTimeFuncBytes))

}