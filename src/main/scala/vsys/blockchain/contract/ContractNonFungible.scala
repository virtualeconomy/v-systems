package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import vsys.blockchain.contract.ContractGen._
import vsys.utils.serialization.Deser

object ContractNonFungible {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1),
    Seq(initFunc),
    Seq(supersedeFunc, issueFunc, sendFunc, transferFunc, depositFunc, withdrawFunc),
    Seq(issuerStateVar.arr, makerStateVar.arr),
    Seq(),
    Seq(triggerTextual, descriptorTextual, stateVarTextual)
  ).right.get

  // StateVar
  val stateVarName = List("issuer", "maker")
  val issuerStateVar: StateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val makerStateVar: StateVar = StateVar(1.toByte, DataType.Address.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // initTrigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq(
    "signer")
  val initDataType: Array[Byte] = Array()
  val initOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(0.toByte),
    cdbvSet ++ Array(issuerStateVar.index, 0.toByte),
    cdbvSet ++ Array(makerStateVar.index, 0.toByte))
  lazy val initFunc: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initOpcs)
  lazy val initFuncBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Functions
  // Supersede
  val supersedeId: Short = 0
  val supersedePara: Seq[String] = Seq("newIssuer",
    "maker")
  val supersedeDataType: Array[Byte] = Array(DataType.Account.id.toByte)
  val supersedeOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrGet ++ Array(makerStateVar.index, 1.toByte),
    assertSigner ++ Array(1.toByte),
    cdbvSet ++ Array(issuerStateVar.index, 0.toByte))
  lazy val supersedeFunc: Array[Byte] = getFunctionBytes(supersedeId, publicFuncType, nonReturnType, supersedeDataType, supersedeOpcs)
  val supersedeFuncBytes: Array[Byte] = textualFunc("supersede", Seq(), supersedePara)

  // Issue
  val issueId: Short = 1
  val issuePara: Seq[String] = Seq("tokenDescription",
    "issuer", "amount", "tokens")
  val issueDataType: Array[Byte] = Array(DataType.ShortText.id.toByte)
  val issueOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(issuerStateVar.index, 1.toByte),
    assertCaller ++ Array(1.toByte),
    cdbvrConstantGet ++ DataEntry(Longs.toByteArray(1), DataType.Amount).bytes ++ Array(2.toByte),
    tdbNewToken ++ Array(2.toByte, 2.toByte, 0.toByte),
    loadTokenNum ++ Array(3.toByte),
    tdbaDeposit ++ Array(1.toByte, 2.toByte, 3.toByte))
  lazy val issueFunc: Array[Byte] = getFunctionBytes(issueId, publicFuncType, nonReturnType, issueDataType, issueOpcs)
  val issueFuncBytes: Array[Byte] = textualFunc("issue", Seq(), issuePara)

  //send
  val sendId: Short = 2
  val sendPara: Seq[String] = Seq("recipient", "tokenIndex",
    "caller" , "amount")
  val sendDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Int32.id.toByte)
  val sendOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(2.toByte),
    cdbvrConstantGet ++ DataEntry(Longs.toByteArray(1), DataType.Amount).bytes ++ Array(3.toByte),
    tdbaTransfer ++ Array(2.toByte, 0.toByte, 3.toByte, 1.toByte))
  lazy val sendFunc: Array[Byte] = getFunctionBytes(sendId, publicFuncType, nonReturnType, sendDataType, sendOpcs)
  val sendFuncBytes: Array[Byte] = textualFunc("send", Seq(), sendPara)

  //transfer
  val transferId: Short = 3
  val transferPara: Seq[String] = Seq("sender", "recipient", "tokenIndex",
    "amount")
  val transferDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Int32.id.toByte)
  val transferOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrConstantGet ++ DataEntry(Longs.toByteArray(1), DataType.Amount).bytes ++ Array(3.toByte),
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 3.toByte, 2.toByte))
  lazy val transferFunc: Array[Byte] = getFunctionBytes(transferId, publicFuncType, nonReturnType, transferDataType, transferOpcs)
  val transferFuncBytes: Array[Byte] = textualFunc("transfer", Seq(), transferPara)

  //deposit
  val depositId: Short = 4
  val depositPara: Seq[String] = Seq("sender", "smart", "tokenIndex",
    "amount")
  val depositDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Int32.id.toByte)
  val depositOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    cdbvrConstantGet ++ DataEntry(Longs.toByteArray(1), DataType.Amount).bytes ++ Array(3.toByte),
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 3.toByte, 2.toByte))
  lazy val depositFunc: Array[Byte] = getFunctionBytes(depositId, publicFuncType, nonReturnType, depositDataType, depositOpcs)
  val depositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  //withdraw
  val withdrawId: Short = 5
  val withdrawPara: Seq[String] = Seq("smart", "recipient", "tokenIndex",
    "amount")
  val withdrawDataType: Array[Byte] = Array(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Int32.id.toByte)
  val withdrawOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(1.toByte),
    cdbvrConstantGet ++ DataEntry(Longs.toByteArray(1), DataType.Amount).bytes ++ Array(3.toByte),
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 3.toByte, 2.toByte))
  lazy val withdrawFunc: Array[Byte] = getFunctionBytes(withdrawId, publicFuncType, nonReturnType, withdrawDataType, withdrawOpcs)
  val withdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  //textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(supersedeFuncBytes, issueFuncBytes, sendFuncBytes,
    transferFuncBytes, depositFuncBytes, withdrawFuncBytes))

}
