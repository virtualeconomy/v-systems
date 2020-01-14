package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.utils.serialization.Deser

object ContractDepositWithdraw {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1),
    Seq(initTrigger, depositTrigger, withdrawTrigger), Seq(),
      Seq(makerStateVar.arr), Seq(), Seq(triggerTextual, descriptorTextual, stateVarTextual)
  ).right.get

  //stateVar
  val stateVarName = List("maker")
  val makerStateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  //initTrigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq(
    "signer")
  val initDataType: Array[Byte] = Array()
  val initOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(0.toByte),
    cdbvSet ++ Array(makerStateVar.index, 0.toByte))
  val initTrigger: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initOpcs)
  val initFuncBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  //depositTrigger
  val depositId: Short = 1
  val depositPara: Seq[String] = Seq("sender", "amount", "tokenId")
  val depositDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val depositOpcs: Seq[Array[Byte]] = Seq()
  val depositTrigger: Array[Byte] = getFunctionBytes(depositId, onDepositTriggerType, nonReturnType, depositDataType, depositOpcs)
  val depositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  //withdrawTrigger
  val withdrawId: Short = 2
  val withdrawPara: Seq[String] = Seq("recipient", "amount", "tokenId")
  val withdrawDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte, DataType.TokenId.id.toByte)
  val withdrawOpcs: Seq[Array[Byte]] = Seq()
  val withdrawTrigger: Array[Byte] = getFunctionBytes(withdrawId, onWithDrawTriggerType, nonReturnType, withdrawDataType, withdrawOpcs)
  val withdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  //Textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes, depositFuncBytes, withdrawFuncBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq())
}