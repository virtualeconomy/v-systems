package vsys.blockchain.contract

import com.google.common.primitives.{Bytes, Ints, Shorts}
import vsys.utils.serialization.Deser

object ContractDepositWithdraw {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1),
    Seq(depositTrigger, withdrawTrigger), Seq(),
    Seq(Array(StateVar.maker, DataType.Address.id.toByte)), Seq(triggerTextual, descriptorTextual, stateVarTextual)
  ).right.get

  object FunId {
    val deposit: Short = 0
    val withdraw: Short = 1
  }

  object ProtoType {
    val depositParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)
    val withdrawParaType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)
  }

  def listOpc(ids: List[Array[Byte]], indexInput: List[Array[Byte]]): Array[Byte] = {
    val length = Shorts.toByteArray((ids.zip(indexInput).map(x => ((x._1 ++ x._2).length + 2).toShort).sum + 2).toShort)
    val numOpc = Shorts.toByteArray(ids.length.toShort)
    val listOpc = ids.zip(indexInput).map(x => Shorts.toByteArray((x._1 ++ x._2).length.toShort) ++ x._1 ++ x._2).toArray.flatten
    Bytes.concat(length, numOpc, listOpc)
  }

  object StateVar {
    val maker: Byte = 0
  }

  object DataStack {
    object depositInput {
      val senderIndex: Byte = 0
      val amountIndex: Byte = 1
    }

    object withdrawInput {
      val receiptIndex: Byte = 0
      val amountIndex: Byte = 1
    }

  }

  object OpcLine {
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
  lazy val onDepositTriggerType: Byte = 0
  lazy val onWithDrawTriggerType: Byte = 1
  lazy val depositTrigger: Array[Byte] = Shorts.toByteArray(FunId.deposit) ++ Array(onDepositTriggerType) ++ protoType(nonReturnType, ProtoType.depositParaType) ++ OpcLine.depositOpcLine
  lazy val withdrawTrigger: Array[Byte] = Shorts.toByteArray(FunId.withdraw) ++ Array(onWithDrawTriggerType) ++ protoType(nonReturnType, ProtoType.withdrawParaType) ++ OpcLine.withdrawOpcLine

  def textualFunc(name: String, ret: Seq[String], para: Seq[String]): Array[Byte] = {
    val funcByte = Deser.serializeArray(Deser.serilizeString(name))
    val retByte = Deser.serializeArray(Deser.serializeArrays(ret.map(x => Deser.serilizeString(x))))
    val paraByte = Deser.serializeArrays(para.map(x => Deser.serilizeString(x)))
    Bytes.concat(funcByte, retByte, paraByte)
  }

  object ParaName {
    val depositPara: Seq[String] = Seq("sender", "amount")
    val withdrawPara: Seq[String]= Seq("recipient", "amount")
  }

  val stateVarName = List("maker")
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  val depositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), ParaName.depositPara)
  val withdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), ParaName.withdrawPara)

  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(depositFuncBytes, withdrawFuncBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq())

}