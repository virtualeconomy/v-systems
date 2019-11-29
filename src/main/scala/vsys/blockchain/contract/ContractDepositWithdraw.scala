package vsys.blockchain.contract

import com.google.common.primitives.{Bytes, Ints, Shorts}
import vsys.blockchain.state.opcdiffs.{CDBVOpcDiff, LoadOpcDiff, OpcDiffer}
import vsys.utils.serialization.Deser

object ContractDepositWithdraw {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1),
    Seq(initTrigger, depositTrigger, withdrawTrigger), Seq(), Seq(),
    Seq(Array(StateVar.maker, DataType.Address.id.toByte)), Seq(triggerTextual, descriptorTextual, stateVarTextual)
  ).right.get

  object FunId {
    val init: Short = 0
    val deposit: Short = 1
    val withdraw: Short = 2
  }

  object ProtoType {
    val initParaType: Array[Byte] = Array()
    val depositParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)
    val withdrawParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)
  }

  def listOpc(ids: List[Array[Byte]], indexInput: List[Array[Byte]]): Array[Byte] = {
    val length = Shorts.toByteArray((ids.zip(indexInput).map(x => ((x._1 ++ x._2).length + 2).toShort).sum + 2).toShort)
    val numOpc = Shorts.toByteArray(ids.length.toShort)
    val listOpc = ids.zip(indexInput).map(x => Shorts.toByteArray((x._1 ++ x._2).length.toShort) ++ x._1 ++ x._2).toArray.flatten
    Bytes.concat(length, numOpc, listOpc)
  }

  object OpcId {
    val opcLoadSigner: Array[Byte] = Array(OpcDiffer.OpcType.LoadOpc.id.toByte, LoadOpcDiff.LoadType.SignerLoad.id.toByte)
    val opcCDBVSet: Array[Byte] = Array(OpcDiffer.OpcType.CDBVOpc.id.toByte, CDBVOpcDiff.CDBVType.SetCDBV.id.toByte)
  }

  object StateVar {
    val maker: Byte = 0
  }

  object DataStack {
    object initInput {
      val issuerLoadIndex: Byte = 0
    }

    object depositInput {
      val senderIndex: Byte = 0
      val amountIndex: Byte = 1
    }

    object withdrawInput {
      val receiptIndex: Byte = 0
      val amountIndex: Byte = 1
    }

  }

  object ListOpc {
    val opcLoadSignerIndex: Array[Byte] = Array(0.toByte)

    val opcCDBVSetMakerIndex: Array[Byte] = Array(StateVar.maker, DataStack.initInput.issuerLoadIndex)

    // init
    val initOpc: List[Array[Byte]] = List(OpcId.opcLoadSigner, OpcId.opcCDBVSet)
    val initOpcIndex: List[Array[Byte]] = List(opcLoadSignerIndex, opcCDBVSetMakerIndex)
  }

  object OpcLine {
    val initOpcLine: Array[Byte] = listOpc(ListOpc.initOpc, ListOpc.initOpcIndex)
    // val depositOpcLine: Array[Byte] = listOpc(ListOpc.depositOpc, ListOpc.depositOpcIndex)
    val depositOpcLine: Array[Byte] = listOpc(List(), List())
    //val withdrawOpcLine: Array[Byte] = listOpc(ListOpc.withdrawOpc, ListOpc.withdrawOpcIndex)
    val withdrawOpcLine: Array[Byte] = listOpc(List(), List())
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
  lazy val initTrigger: Array[Byte] = Shorts.toByteArray(FunId.init) ++ Array(onInitTriggerType) ++ protoType(nonReturnType, ProtoType.initParaType) ++ OpcLine.initOpcLine
  lazy val depositTrigger: Array[Byte] = Shorts.toByteArray(FunId.deposit) ++ Array(onDepositTriggerType) ++ protoType(nonReturnType, ProtoType.depositParaType) ++ OpcLine.depositOpcLine
  lazy val withdrawTrigger: Array[Byte] = Shorts.toByteArray(FunId.withdraw) ++ Array(onWithDrawTriggerType) ++ protoType(nonReturnType, ProtoType.withdrawParaType) ++ OpcLine.withdrawOpcLine

  def textualFunc(name: String, ret: Seq[String], para: Seq[String]): Array[Byte] = {
    val funcByte = Deser.serializeArray(Deser.serilizeString(name))
    val retByte = Deser.serializeArray(Deser.serializeArrays(ret.map(x => Deser.serilizeString(x))))
    val paraByte = Deser.serializeArrays(para.map(x => Deser.serilizeString(x)))
    Bytes.concat(funcByte, retByte, paraByte)
  }

  object ParaName {
    val initPara: Seq[String] = Seq("signer")
    val depositPara: Seq[String] = Seq("sender", "amount")
    val withdrawPara: Seq[String] = Seq("recipient", "amount")
  }

  val stateVarName = List("maker")
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  val initFuncBytes: Array[Byte] = textualFunc("init", Seq(), ParaName.initPara)
  val depositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), ParaName.depositPara)
  val withdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), ParaName.withdrawPara)

  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes, depositFuncBytes, withdrawFuncBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq())

}