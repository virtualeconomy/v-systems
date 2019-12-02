package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.utils.serialization.Deser

object ContractLock {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger), Seq(lockFunc),
    Seq(makerStateVar.arr, tokenIdStateVar.arr),
    Seq(balanceMap.arr, lockTimeMap.arr),
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
  ).right.get

  // State Var
  val stateVarName = List("maker", "tokenId")
  val makerStateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val tokenIdStateVar = StateVar(1.toByte, DataType.TokenId.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // State Map
  val stateMapName = List("balance", "lockTime")
  val balanceMap = StateMap(0.toByte, DataType.Address.id.toByte, DataType.Amount.id.toByte)
  val lockTimeMap = StateMap(1.toByte, DataType.Address.id.toByte, DataType.Timestamp.id.toByte)
  lazy val stateMapTextual: Array[Byte] = Deser.serializeArrays(stateMapName.map(x => Deser.serilizeString(x)))

  // Initialization Trigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq("tokenId",
                                  "signer")
  val initDataType: Array[Byte] = Array(DataType.TokenId.id.toByte)
  val initTriggerOpcs: Seq[Seq[Array[Byte]]] = Seq(
    Seq(loadSinger, Array(1.toByte)),
    Seq(cdbvSet, Array(makerStateVar.index, 1.toByte)),
    Seq(cdbvSet, Array(tokenIdStateVar.index, 0.toByte))
  )
  lazy val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initTriggerOpcs)
  val initFuncBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Deposit Trigger
  val depositId: Short = 1
  val depositPara: Seq[String] = Seq("depositor", "amount", "tokenId",
                                     "contractTokenId")
  val depositDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositTriggerOpcs: Seq[Seq[Array[Byte]]] = Seq(
    Seq(assertCaller, Array(0.toByte)),
    Seq(cdbvrGet, Array(tokenIdStateVar.index, 3.toByte)),
    Seq(assertEqual, Array(2.toByte, 3.toByte)),
    Seq(cdbvMapValAdd, Array(balanceMap.index, 0.toByte, 1.toByte))
  )
  lazy val depositTrigger: Array[Byte] = getFunctionBytes(depositId, onDepositTriggerType, nonReturnType, depositDataType, depositTriggerOpcs)
  val depositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  // WithDraw Trigger
  val withdrawId: Short = 2
  val withdrawPara: Seq[String] = Seq("withdrawer", "amount", "tokenId",
                                      "contractTokenId", "lastBlockTime", "lockedTime", "compareResult")
  val withdrawDataType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val withdrawTriggerOpcs: Seq[Seq[Array[Byte]]] = Seq(
    Seq(assertCaller, Array(0.toByte)),
    Seq(cdbvrGet, Array(tokenIdStateVar.index, 3.toByte)),
    Seq(assertEqual, Array(2.toByte, 3.toByte)),
    Seq(loadTimestamp, Array(4.toByte)),
    Seq(cdbvrMapGetOrDefault, Array(lockTimeMap.index, 0.toByte, 5.toByte)),
    Seq(compareGreater, Array(4.toByte, 5.toByte, 6.toByte)),
    Seq(assertTrue, Array(6.toByte)),
    Seq(cdbvMapValMinus, Array(balanceMap.index, 0.toByte, 1.toByte))
  )
  lazy val withdrawTrigger: Array[Byte] = getFunctionBytes(withdrawId, onWithDrawTriggerType, nonReturnType, withdrawDataType, depositTriggerOpcs)
  val withdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  // Lock Function
  val lockId: Short = 0
  val lockPara: Seq[String] = Seq("timestamp",
                                  "caller", "lockedTime", "compareResult")
  val lockDataType: Array[Byte] = Array(DataType.Timestamp.id.toByte)
  val lockFunctionOpcs: Seq[Seq[Array[Byte]]] = Seq(
    Seq(loadCaller, Array(1.toByte)),
    Seq(cdbvrMapGetOrDefault, Array(lockTimeMap.index, 1.toByte, 2.toByte)),
    Seq(compareGreater, Array(0.toByte, 2.toByte, 3.toByte)),
    Seq(assertTrue, Array(3.toByte)),
    Seq(cdbvMapSet, Array(lockTimeMap.index, 1.toByte, 0.toByte))
  )
  lazy val lockFunc: Array[Byte] = getFunctionBytes(lockId, publicFuncType, nonReturnType, lockDataType, lockFunctionOpcs)
  val lockFuncBytes: Array[Byte] = textualFunc("lock", Seq(), lockPara)

  // Gen Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes, depositFuncBytes, withdrawFuncBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(lockFuncBytes))

}