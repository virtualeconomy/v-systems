package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractPermitted {
  lazy val contract: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1),
    Seq(initFunc),
    Seq(supersedeFunc, issueFunc, destroyFunc, splitFunc, sendFunc, transferFunc, depositFunc, withdrawFunc,
      totalSupplyFunc, maxSupplyFunc, balanceOfFunc, getIssuerFunc),
    Seq(issuerStateVar.arr, makerStateVar.arr),
    Seq(),
    Seq(triggerTextual, descriptorTextual, stateVarTextual)
  ).explicitGet()

  lazy val contractWithoutSplit: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(1),
    Seq(initFunc),
    Seq(supersedeFuncWithoutSplit, issueFuncWithoutSplit, destroyFuncWithoutSplit, sendFuncWithoutSplit,
      transferFuncWithoutSplit, depositFuncWithoutSplit, withdrawFuncWithoutSplit, totalSupplyFuncWithoutSplit,
      maxSupplyFuncWithoutSplit, balanceOfFuncWithoutSplit, getIssuerFuncWithoutSplit),
    Seq(issuerStateVar.arr, makerStateVar.arr),
    Seq(),
    Seq(triggerTextual, descriptorTextualWithoutSplit, stateVarTextual)
  ).right.get

  // StateVar
  val stateVarName = List("issuer", "maker")
  val issuerStateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val makerStateVar = StateVar(1.toByte, DataType.Address.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // initTrigger
  val initId: Short = 0
  val initPara: Seq[String] = Seq("max", "unity", "tokenDescription",
    "signer")
  val initDataType = Array(DataType.Amount.id.toByte, DataType.Amount.id.toByte, DataType.ShortText.id.toByte)
  val initOpcs: Seq[Array[Byte]] = Seq(
    loadSigner ++ Array(3.toByte),
    cdbvSet ++ Array(issuerStateVar.index, 3.toByte),
    cdbvSet ++ Array(makerStateVar.index, 3.toByte),
    tdbNewToken ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val initFunc: Array[Byte] = getFunctionBytes(initId, onInitTriggerType, nonReturnType, initDataType, initOpcs)
  lazy val initFuncBytes: Array[Byte] = textualFunc("init", Seq(), initPara)

  // Functions
  // Supersede
  val supersedeId: Short = 0
  val supersedeIdWithoutSplit: Short = 0
  val supersedePara: Seq[String] = Seq("newIssuer",
    "maker")
  val supersedeDataType = Array(DataType.Account.id.toByte)
  val supersedeOpcs: Seq[Array[Byte]] =  Seq(
    cdbvrGet ++ Array(makerStateVar.index, 1.toByte),
    assertSigner ++ Array(1.toByte),
    cdbvSet ++ Array(issuerStateVar.index, 0.toByte))
  lazy val supersedeFunc: Array[Byte] = getFunctionBytes(supersedeId, publicFuncType, nonReturnType, supersedeDataType, supersedeOpcs)
  lazy val supersedeFuncWithoutSplit: Array[Byte] = getFunctionBytes(supersedeIdWithoutSplit, publicFuncType, nonReturnType, supersedeDataType, supersedeOpcs)
  val supersedeFuncBytes: Array[Byte] = textualFunc("supersede", Seq(), supersedePara)

  // Issue
  val issueId: Short = 1
  val issueIdWithoutSplit: Short = 1
  val issuePara: Seq[String] = Seq("amount",
    "issuer")
  val issueDataType: Array[Byte] = Array(DataType.Amount.id.toByte)
  val issueOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(issuerStateVar.index, 1.toByte),
    assertCaller ++ Array(1.toByte),
    tdbaDeposit ++ Array(1.toByte, 0.toByte))
  lazy val issueFunc: Array[Byte] = getFunctionBytes(issueId, publicFuncType, nonReturnType, issueDataType, issueOpcs)
  lazy val issueFuncWithoutSplit: Array[Byte] = getFunctionBytes(issueIdWithoutSplit, publicFuncType, nonReturnType, issueDataType, issueOpcs)
  val issueFuncBytes: Array[Byte] = textualFunc("issue", Seq(), issuePara)

  //destroy
  val destroyId: Short = 2
  val destroyIdWithoutSplit: Short = 2
  val destroyPara: Seq[String] = Seq("amount",
    "issuer")
  val destroyDataType: Array[Byte] = Array(DataType.Amount.id.toByte)
  val destroyOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(issuerStateVar.index, 1.toByte),
    assertCaller ++ Array(1.toByte),
    tdbaWithdraw ++ Array(1.toByte, 0.toByte))
  lazy val destroyFunc: Array[Byte] = getFunctionBytes(destroyId, publicFuncType, nonReturnType, destroyDataType, destroyOpcs)
  lazy val destroyFuncWithoutSplit: Array[Byte] = getFunctionBytes(destroyIdWithoutSplit, publicFuncType, nonReturnType, destroyDataType, destroyOpcs)
  val destroyFuncBytes: Array[Byte] = textualFunc("destroy", Seq(), destroyPara)

  // split
  val splitId: Short = 3
  val splitPara: Seq[String] = Seq("newUnity",
    "issuer")
  val splitDataType: Array[Byte] = Array(DataType.Amount.id.toByte)
  val splitOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(issuerStateVar.index, 1.toByte),
    assertCaller ++ Array(1.toByte),
    tdbSplit ++ Array(0.toByte))
  lazy val splitFunc: Array[Byte] = getFunctionBytes(splitId, publicFuncType, nonReturnType, splitDataType, splitOpcs)
  val splitFuncBytes: Array[Byte] = textualFunc("split", Seq(), splitPara)

  //send
  val sendId: Short = 4
  val sendIdWithoutSplit: Short = 3
  val sendPara: Seq[String] = Seq("recipient", "amount",
    "caller")
  val sendDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val sendOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(2.toByte),
    tdbaTransfer ++ Array(2.toByte, 0.toByte, 1.toByte))
  lazy val sendFunc: Array[Byte] = getFunctionBytes(sendId, publicFuncType, nonReturnType, sendDataType, sendOpcs)
  lazy val sendFuncWithoutSplit: Array[Byte] = getFunctionBytes(sendIdWithoutSplit, publicFuncType, nonReturnType, sendDataType, sendOpcs)
  val sendFuncBytes: Array[Byte] = textualFunc("send", Seq(), sendPara)

  //transfer
  val transferId: Short = 5
  val transferIdWithoutSplit: Short = 4
  val transferPara: Seq[String] = Seq("sender", "recipient", "amount")
  val transferDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val transferOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val transferFunc: Array[Byte] = getFunctionBytes(transferId, publicFuncType, nonReturnType, transferDataType, transferOpcs)
  lazy val transferFuncWithoutSplit: Array[Byte] = getFunctionBytes(transferIdWithoutSplit, publicFuncType, nonReturnType, transferDataType, transferOpcs)
  val transferFuncBytes: Array[Byte] = textualFunc("transfer", Seq(), transferPara)

  //deposit
  val depositId: Short = 6
  val depositIdWithoutSplit: Short = 5
  val depositPara: Seq[String] = Seq("sender", "smart", "amount")
  val depositDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Amount.id.toByte)
  val depositOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte),
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val depositFunc: Array[Byte] = getFunctionBytes(depositId, publicFuncType, nonReturnType, depositDataType, depositOpcs)
  lazy val depositFuncWithoutSplit: Array[Byte] = getFunctionBytes(depositIdWithoutSplit, publicFuncType, nonReturnType, depositDataType, depositOpcs)
  val depositFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), depositPara)

  //withdraw
  val withdrawId: Short = 7
  val withdrawIdWithoutSplit: Short = 6
  val withdrawPara: Seq[String] = Seq("smart", "recipient", "amount")
  val withdrawDataType: Array[Byte] = Array(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)
  val withdrawOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(1.toByte),
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val withdrawFunc: Array[Byte] = getFunctionBytes(withdrawId, publicFuncType, nonReturnType, withdrawDataType, withdrawOpcs)
  lazy val withdrawFuncWithoutSplit: Array[Byte] = getFunctionBytes(withdrawIdWithoutSplit, publicFuncType, nonReturnType, withdrawDataType, withdrawOpcs)
  val withdrawFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawPara)

  //totalSupply
  val totalSupplyId: Short = 8
  val totalSupplyIdWithoutSplit: Short = 7
  val totalSupplyPara: Seq[String] = Seq(
    "total")
  val totalSupplyDataType: Array[Byte] = Array()
  val totalSupplyOpcs: Seq[Array[Byte]] = Seq(
    tdbroOpcTotal ++ Array(0.toByte),
    returnValue ++ Array(0.toByte))
  lazy val totalSupplyFunc: Array[Byte] = getFunctionBytes(totalSupplyId, publicFuncType, Array(DataType.Amount.id.toByte), totalSupplyDataType, totalSupplyOpcs)
  lazy val totalSupplyFuncWithoutSplit: Array[Byte] = getFunctionBytes(totalSupplyIdWithoutSplit, publicFuncType, Array(DataType.Amount.id.toByte), totalSupplyDataType, totalSupplyOpcs)
  val totalSupplyFuncBytes: Array[Byte] = textualFunc("totalSupply", Seq("total"), totalSupplyPara)

  //maxSupply
  val maxSupplyId: Short = 9
  val maxSupplyIdWithoutSplit: Short = 8
  val maxSupplyPara: Seq[String] = Seq(
    "max")
  val maxSupplyDataType: Array[Byte] = Array()
  val maxSupplyOpcs: Seq[Array[Byte]] = Seq(
    tdbroOpcMax ++ Array(0.toByte),
    returnValue ++ Array(0.toByte))
  lazy val maxSupplyFunc: Array[Byte] = getFunctionBytes(maxSupplyId, publicFuncType, Array(DataType.Amount.id.toByte), maxSupplyDataType, maxSupplyOpcs)
  lazy val maxSupplyFuncWithoutSplit: Array[Byte] = getFunctionBytes(maxSupplyIdWithoutSplit, publicFuncType, Array(DataType.Amount.id.toByte), maxSupplyDataType, maxSupplyOpcs)
  val maxSupplyFuncBytes: Array[Byte] = textualFunc("maxSupply", Seq("max"), maxSupplyPara)

  //balanceOf
  val balanceOfId: Short = 10
  val balanceOfIdWithoutSplit: Short = 9
  val balanceOfPara: Seq[String] = Seq("address",
    "balance")
  val balanceOfDataType: Array[Byte] = Array(DataType.Account.id.toByte)
  val balanceOfOpcs: Seq[Array[Byte]] = Seq(
    tdbarBalance ++ Array(0.toByte, 1.toByte),
    returnValue ++ Array(1.toByte))
  lazy val balanceOfFunc: Array[Byte] = getFunctionBytes(balanceOfId, publicFuncType, Array(DataType.Amount.id.toByte), balanceOfDataType, balanceOfOpcs)
  lazy val balanceOfFuncWithoutSplit: Array[Byte] = getFunctionBytes(balanceOfIdWithoutSplit, publicFuncType, Array(DataType.Amount.id.toByte), balanceOfDataType, balanceOfOpcs)
  val balanceOfFuncBytes: Array[Byte] = textualFunc("balanceOf", Seq("balance"), balanceOfPara)

  //getIssuer
  val getIssuerId: Short = 11
  val getIssuerIdWithoutSplit: Short = 10
  val getIssuerPara: Seq[String] = Seq(
    "issuer")
  val getIssuerDataType: Array[Byte] = Array()
  val getIssuerOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(issuerStateVar.index, 0.toByte),
    returnValue ++ Array(0.toByte))
  lazy val getIssuerFunc: Array[Byte] = getFunctionBytes(getIssuerId, publicFuncType, Array(DataType.Account.id.toByte), getIssuerDataType, getIssuerOpcs)
  lazy val getIssuerFuncWithoutSplit: Array[Byte] = getFunctionBytes(getIssuerIdWithoutSplit, publicFuncType, Array(DataType.Account.id.toByte), getIssuerDataType, getIssuerOpcs)
  val getIssuerFuncBytes: Array[Byte] = textualFunc("getIssuer", Seq("issuer"), getIssuerPara)

  //textual
  lazy val triggerTextual: Array[Byte] = Deser.serializeArrays(Seq(initFuncBytes))
  lazy val descriptorTextual: Array[Byte] = Deser.serializeArrays(Seq(supersedeFuncBytes, issueFuncBytes,
    destroyFuncBytes, splitFuncBytes, sendFuncBytes, transferFuncBytes, depositFuncBytes, withdrawFuncBytes,
    totalSupplyFuncBytes, maxSupplyFuncBytes, balanceOfFuncBytes, getIssuerFuncBytes))
  lazy val descriptorTextualWithoutSplit: Array[Byte] = Deser.serializeArrays(Seq(supersedeFuncBytes, issueFuncBytes,
    destroyFuncBytes, sendFuncBytes, transferFuncBytes, depositFuncBytes, withdrawFuncBytes,
    totalSupplyFuncBytes, maxSupplyFuncBytes, balanceOfFuncBytes, getIssuerFuncBytes))
}
