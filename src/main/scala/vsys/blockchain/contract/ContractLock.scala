package vsys.blockchain.contract

import com.google.common.primitives.{Bytes, Ints, Shorts}
import vsys.blockchain.state.opcdiffs._
import vsys.utils.serialization.Deser

object ContractLock {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(initTrigger, depositTrigger, withdrawTrigger), Seq(lockFunc),
    Seq(Array(StateVar.maker, DataType.Address.id.toByte), Array(StateVar.tokenId, DataType.TokenId.id.toByte)),
    Seq(Array(StateMap.balance, DataType.Address.id.toByte, DataType.Amount.id.toByte), Array(StateMap.time, DataType.Address.id.toByte, DataType.Timestamp.id.toByte)),
    Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)
  ).right.get

  object FunId {
    val init: Short = 0
    val deposit: Short = 1
    val withdraw: Short = 2

    val lock: Short = 0
  }

  object ProtoType {
    val initParaType: Array[Byte] = Array(DataType.TokenId.id.toByte)
    val depositParaType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
    val withdrawParaType: Array[Byte] = Array(DataType.Address.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
    val lockParaType: Array[Byte] = Array(DataType.Timestamp.id.toByte)
  }

  def listOpc(ids: List[Array[Byte]], indexInput: List[Array[Byte]]): Array[Byte] = {
    val length = Shorts.toByteArray((ids.zip(indexInput).map(x => ((x._1 ++ x._2).length + 2).toShort).sum + 2).toShort)
    val numOpc = Shorts.toByteArray(ids.length.toShort)
    val listOpc = ids.zip(indexInput).map(x => Shorts.toByteArray((x._1 ++ x._2).length.toShort) ++ x._1 ++ x._2).toArray.flatten
    Bytes.concat(length, numOpc, listOpc)
  }

  object OpcId {
    val opcAssertIsCallerOrigin: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.IsCallerOriginAssert.id.toByte)
    val opcAssertIsTokenIdOrigin: Array[Byte] = Array(OpcDiffer.OpcType.AssertOpc.id.toByte, AssertOpcDiff.AssertType.EqAssert.id.toByte)

    val opcLoadSigner: Array[Byte] = Array(OpcDiffer.OpcType.LoadOpc.id.toByte, LoadOpcDiff.LoadType.SignerLoad.id.toByte)
    val opcLoadTokenId: Array[Byte] = Array(OpcDiffer.OpcType.LoadOpc.id.toByte, LoadOpcDiff.LoadType.SignerLoad.id.toByte)

    val opcCDBVSet: Array[Byte] = Array(OpcDiffer.OpcType.CDBVOpc.id.toByte, CDBVOpcDiff.CDBVType.SetCDBV.id.toByte)

    val opcCDBVRGet: Array[Byte] = Array(OpcDiffer.OpcType.CDBVROpc.id.toByte, CDBVROpcDiff.CDBVRType.GetCDBVR.id.toByte)

  }

  object StateVar {
    val maker: Byte = 0
    val tokenId: Byte = 1
  }

  object StateMap {
    val balance: Byte = 0
    val time: Byte = 1
  }

  object DataStack {
    object initInput {
      val tokenIdIndex: Byte = 0
      val issuerLoadIndex: Byte = 1
    }

    object depositInput {
      val senderIndex: Byte = 0
      val amountIndex: Byte = 1
      val tokenIdIndex: Byte = 2
      val tokenIdGetIndex: Byte = 3
    }

    object withdrawInput {
      val receiptIndex: Byte = 0
      val amountIndex: Byte = 1
      val tokenIdIndex: Byte = 2
      val tokenIdGetIndex: Byte = 3
    }

    object lockInput {
      val timestampIndex: Byte = 0
    }

  }

  object ListOpc {
    // need to update
    val opcLoadSignerIndex: Array[Byte] = Array(1.toByte)

    val opcCDBVSetMakerIndex: Array[Byte] = Array(StateVar.maker, DataStack.initInput.issuerLoadIndex)
    val opcCDBVSetTokenIdIndex: Array[Byte] = Array(StateVar.tokenId, DataStack.initInput.tokenIdIndex)

    val opcCDBVRGetTokenIdDepositIndex: Array[Byte] = Array(StateVar.tokenId, DataStack.depositInput.tokenIdIndex)
    val opcCDBVRGetTokenIdWithdrawIndex: Array[Byte] = Array(StateVar.tokenId, DataStack.withdrawInput.tokenIdIndex)

    val opcAssertIsCallerOriginDepositIndex: Array[Byte] = Array(DataStack.depositInput.senderIndex)
    val opcAssertIsCallerOriginWithdrawIndex: Array[Byte] = Array(DataStack.withdrawInput.receiptIndex)
    val opcAssertIsTokenIdOriginDepositIndex: Array[Byte] = Array(DataStack.depositInput.tokenIdIndex, DataStack.depositInput.tokenIdGetIndex)
    val opcAssertIsTokenIdOriginWithdrawIndex: Array[Byte] = Array(DataStack.withdrawInput.tokenIdIndex, DataStack.withdrawInput.tokenIdGetIndex)


    // init
    val initOpc: List[Array[Byte]] = List(OpcId.opcLoadSigner, OpcId.opcCDBVSet, OpcId.opcCDBVSet)
    val initOpcIndex: List[Array[Byte]] = List(opcLoadSignerIndex, opcCDBVSetMakerIndex, opcCDBVSetTokenIdIndex)
    // deposit
    val depositOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcCDBVRGet, OpcId.opcAssertIsTokenIdOrigin)
    val depositOpcIndex: List[Array[Byte]] = List(opcAssertIsCallerOriginDepositIndex, opcCDBVRGetTokenIdDepositIndex, opcAssertIsTokenIdOriginDepositIndex)
    // withdraw
    val withdrawOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcCDBVRGet, OpcId.opcAssertIsTokenIdOrigin)
    val withdrawOpcIndex: List[Array[Byte]] = List(opcAssertIsCallerOriginWithdrawIndex, opcCDBVRGetTokenIdWithdrawIndex, opcAssertIsTokenIdOriginWithdrawIndex)
    // lock
    val lockOpc: List[Array[Byte]] = List(OpcId.opcAssertIsCallerOrigin, OpcId.opcCDBVRGet, OpcId.opcAssertIsTokenIdOrigin)
    val lockOpcIndex: List[Array[Byte]] = List(opcAssertIsCallerOriginWithdrawIndex, opcCDBVRGetTokenIdWithdrawIndex, opcAssertIsTokenIdOriginWithdrawIndex)
  }

  object OpcLine {
    // need to update
    val initOpcLine: Array[Byte] = listOpc(ListOpc.initOpc, ListOpc.initOpcIndex)
    val depositOpcLine: Array[Byte] = listOpc(ListOpc.depositOpc, ListOpc.depositOpcIndex)
    val withdrawOpcLine: Array[Byte] = listOpc(ListOpc.withdrawOpc, ListOpc.withdrawOpcIndex)
    val lockOpcLine: Array[Byte] = listOpc(ListOpc.withdrawOpc, ListOpc.withdrawOpcIndex)
  }

  def protoType(listReturnType: Array[Byte], listParaTypes: Array[Byte]): Array[Byte] = {
    val retType = Deser.serializeArray(listReturnType)
    val paraType = Deser.serializeArray(listParaTypes)
    Bytes.concat(retType, paraType)
  }

  lazy val nonReturnType: Array[Byte] = Array[Byte]()
  lazy val onInitTriggerType: Byte = 0
  lazy val onDepositTriggerType: Byte = 1
  lazy val onWithDrawTriggerType: Byte = 2
  lazy val publicFuncType: Byte = 0
  lazy val initTrigger: Array[Byte] = Shorts.toByteArray(FunId.init) ++ Array(onInitTriggerType) ++ protoType(nonReturnType, ProtoType.initParaType) ++ OpcLine.initOpcLine
  lazy val depositTrigger: Array[Byte] = Shorts.toByteArray(FunId.deposit) ++ Array(onDepositTriggerType) ++ protoType(nonReturnType, ProtoType.depositParaType) ++ OpcLine.depositOpcLine
  lazy val withdrawTrigger: Array[Byte] = Shorts.toByteArray(FunId.withdraw) ++ Array(onWithDrawTriggerType) ++ protoType(nonReturnType, ProtoType.withdrawParaType) ++ OpcLine.withdrawOpcLine
  lazy val lockFunc: Array[Byte] = Shorts.toByteArray(FunId.lock) ++ Array(publicFuncType) ++ protoType(nonReturnType, ProtoType.lockParaType) ++ OpcLine.lockOpcLine

  def textualFunc(name: String, ret: Seq[String], para: Seq[String]): Array[Byte] = {
    val funcByte = Deser.serializeArray(Deser.serilizeString(name))
    val retByte = Deser.serializeArray(Deser.serializeArrays(ret.map(x => Deser.serilizeString(x))))
    val paraByte = Deser.serializeArrays(para.map(x => Deser.serilizeString(x)))
    Bytes.concat(funcByte, retByte, paraByte)
  }

  object ParaName {
    val initPara: Seq[String] = Seq("tokenId")
    val depositPara: Seq[String] = Seq("depositor", "amount", "tokenId")
    val withdrawPara: Seq[String] = Seq("withdrawer", "amount", "tokenId")
    val lockPara: Seq[String] = Seq("timestamp")
  }

  val stateVarName = List("tokenId")
  val stateMapName = List("balance", "lockTime")
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))
  lazy val stateMapTextual: Array[Byte] = Deser.serializeArrays(stateMapName.map(x => Deser.serilizeString(x)))

  val initFuncBytes: Array[Byte] = textualFunc("init", Seq(), ParaName.initPara)
  val depositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), ParaName.depositPara)
  val withdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), ParaName.withdrawPara)
  val lockFuncBytes: Array[Byte] = textualFunc("lock", Seq(), ParaName.lockPara)

  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes, depositFuncBytes, withdrawFuncBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(lockFuncBytes))

}