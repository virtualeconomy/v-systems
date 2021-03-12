package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen._
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractTokenV2 {
  lazy val contractTokenWhiteList: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(),
    Seq(),
    Seq(issuerStateVar.arr, makerStateVar.arr),
    Seq(listMap.arr),
    Seq()
  ).explicitGet()

  lazy val contractTokenBlackList: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(),
    Seq(),
    Seq(issuerStateVar.arr, makerStateVar.arr),
    Seq(listMap.arr),
    Seq()
  ).explicitGet()

  // StateVar
  val issuerStateVar: StateVar  = StateVar(0.toByte, DataType.Address.id.toByte)
  val makerStateVar: StateVar   = StateVar(1.toByte, DataType.Address.id.toByte)
  lazy val stateVarTextual: Array[Byte] = ContractPermitted.stateVarTextual

  // State Map
  val stateMapWhitelist    = List("whitelist", "userAccount", "isInList")
  val stateMapBlacklist    = List("blacklist", "userAccount", "isInList")
  val listMap: StateMap    = StateMap(0.toByte, DataType.Account.id.toByte, DataType.Boolean.id.toByte)

  // initTrigger
  lazy val initFunc: Array[Byte] = ContractPermitted.initFunc
  lazy val initFuncBytes: Array[Byte] = ContractPermitted.initFuncBytes

  // Functions
  // Supersede
  lazy val supersedeFunc: Array[Byte] = ContractPermitted.supersedeFunc
  val supersedeFuncBytes: Array[Byte] = ContractPermitted.supersedeFuncBytes

  // Issue
  lazy val issueFunc: Array[Byte] = ContractPermitted.issueFunc
  val issueFuncBytes: Array[Byte] = ContractPermitted.issueFuncBytes

  //destroy
  lazy val destroyFunc: Array[Byte] = ContractPermitted.depositFunc
  val destroyFuncBytes: Array[Byte] = ContractPermitted.depositFuncBytes

  // Update List
  val updateListId: Short = 3
  val updateListPara: Seq[String] = Seq("userAccount",
    "issuer", "valueTrue")
  val updateListDataType: Array[Byte] = Array(DataType.Account.id.toByte)
  val updateListOpcs: Seq[Array[Byte]] = Seq(
    cdbvrGet ++ Array(issuerStateVar.index, 1.toByte),
    assertCaller ++ Array(1.toByte),
    basicConstantGet ++ DataEntry(Array(1.toByte), DataType.Boolean).bytes ++ Array(2.toByte),
    cdbvMapSet ++ Array(listMap.index, 0.toByte, 2.toByte)
  )
  lazy val updateListFunc: Array[Byte] = getFunctionBytes(updateListId, publicFuncType, nonReturnType, updateListDataType, updateListOpcs)
  val updateListFuncBytes: Array[Byte] = textualFunc("updateList", Seq(), updateListPara)

  private def whitelistCheck(sender: Byte, recipient: Byte): Seq[Array[Byte]] =
    Seq(
      cdbvrMapGetOrDefault ++ Array(listMap.index, sender, 3.toByte),
      assertTrue ++ Array(3.toByte),
      cdbvrMapGetOrDefault ++ Array(listMap.index, recipient, 4.toByte),
      assertTrue ++ Array(4.toByte)
    )

  private def blacklistCheck(sender: Byte, recipient: Byte): Seq[Array[Byte]] =
    Seq(
      basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(3.toByte),
      cdbvrMapGetOrDefault ++ Array(listMap.index, sender, 4.toByte),
      assertEqual ++ Array(4.toByte, 3.toByte),
      cdbvrMapGetOrDefault ++ Array(listMap.index, recipient, 5.toByte),
      assertEqual ++ Array(5.toByte, 3.toByte)
    )

  // Send
  val sendId: Short = 4
  val sendDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Amount.id.toByte)

  // Whitelist
  val sendWhitelistPara: Seq[String] = Seq("recipient", "amount",
    "caller", "isSenderInList", "isRecipientInList")
  val sendWhitelistOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(2.toByte)) ++ whitelistCheck(2.toByte, 0.toByte) ++ Seq(
    tdbaTransfer ++ Array(2.toByte, 0.toByte, 1.toByte)
  )
  lazy val sendWhitelistFunc: Array[Byte] = getFunctionBytes(sendId, publicFuncType, nonReturnType, sendDataType, sendWhitelistOpcs)
  val sendWhitelistFuncBytes: Array[Byte] = textualFunc("send", Seq(), sendWhitelistPara)

  // Blacklist
  val sendBlacklistPara: Seq[String] = Seq("recipient", "amount",
    "caller", "valueFalse", "isSenderInList", "isRecipientInList")
  val sendBlacklistOpcs: Seq[Array[Byte]] = Seq(
    loadCaller ++ Array(2.toByte)) ++ blacklistCheck(2.toByte, 0.toByte) ++ Seq(
    tdbaTransfer ++ Array(2.toByte, 0.toByte, 1.toByte)
  )
  lazy val sendBlacklistFunc: Array[Byte] = getFunctionBytes(sendId, publicFuncType, nonReturnType, sendDataType, sendBlacklistOpcs)
  val sendBlacklistFuncBytes: Array[Byte] = textualFunc("send", Seq(), sendBlacklistPara)

  // Transfer
  val transferId: Short = 5
  val transferDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)

  // Whitelist
  val transferWhitelistPara: Seq[String] = Seq("sender", "recipient", "amount",
    "isSenderInList", "isRecipientInList")
  val transferWhitelistOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte)) ++ whitelistCheck(0.toByte, 1.toByte) ++ Seq(
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val transferWhitelistFunc: Array[Byte] = getFunctionBytes(transferId, publicFuncType, nonReturnType, transferDataType, transferWhitelistOpcs)
  val transferWhitelistFuncBytes: Array[Byte] = textualFunc("transfer", Seq(), transferWhitelistPara)

  // Blacklist
  val transferBlacklistPara: Seq[String] = Seq("sender", "recipient", "amount",
    "valueFalse", "isSenderInList", "isRecipientInList")
  val transferBlacklistOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte)) ++ blacklistCheck(0.toByte, 1.toByte) ++ Seq(
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val transferBlacklistFunc: Array[Byte] = getFunctionBytes(transferId, publicFuncType, nonReturnType, transferDataType, transferBlacklistOpcs)
  val transferBlacklistFuncBytes: Array[Byte] = textualFunc("transfer", Seq(), transferBlacklistPara)

  // Deposit
  val depositId: Short = 6
  val depositDataType: Array[Byte] = Array(DataType.Account.id.toByte, DataType.ContractAccount.id.toByte, DataType.Amount.id.toByte)

  // Whitelist
  val depositWhitelistPara: Seq[String] = Seq("sender", "smart", "amount",
    "isSenderInList", "isRecipientInList")
  val depositWhitelistOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte)) ++ whitelistCheck(0.toByte, 1.toByte) ++ Seq(
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val depositWhitelistFunc: Array[Byte] = getFunctionBytes(depositId, publicFuncType, nonReturnType, depositDataType, depositWhitelistOpcs)
  val depositWhitelistFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), depositWhitelistPara)

  // Blacklist
  val depositBlacklistPara: Seq[String] = Seq("sender", "smart", "amount",
    "valueFalse", "isSenderInList", "isRecipientInList")
  val depositBlacklistOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(0.toByte)) ++ blacklistCheck(0.toByte, 1.toByte) ++ Seq(
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val depositBlacklistFunc: Array[Byte] = getFunctionBytes(depositId, publicFuncType, nonReturnType, depositDataType, depositBlacklistOpcs)
  val depositBlacklistFuncBytes: Array[Byte] = textualFunc("deposit", Seq(), depositBlacklistPara)

  // Withdraw
  val withdrawId: Short = 7
  val withdrawDataType: Array[Byte] = Array(DataType.ContractAccount.id.toByte, DataType.Account.id.toByte, DataType.Amount.id.toByte)

  // Whitelist
  val withdrawWhitelistPara: Seq[String] = Seq("smart", "recipient", "amount",
    "isSenderInList", "isRecipientInList")
  val withdrawWhitelistOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(1.toByte)) ++ whitelistCheck(0.toByte, 1.toByte) ++ Seq(
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val withdrawWhitelistFunc: Array[Byte] = getFunctionBytes(withdrawId, publicFuncType, nonReturnType, withdrawDataType, withdrawWhitelistOpcs)
  val withdrawWhitelistFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawWhitelistPara)

  // Blacklist
  val withdrawBlacklistPara: Seq[String] = Seq("smart", "recipient", "amount",
    "valueFalse", "isSenderInList", "isRecipientInList")
  val withdrawBlacklistOpcs: Seq[Array[Byte]] = Seq(
    assertCaller ++ Array(1.toByte)) ++ blacklistCheck(0.toByte, 1.toByte) ++ Seq(
    tdbaTransfer ++ Array(0.toByte, 1.toByte, 2.toByte))
  lazy val withdrawBlacklistFunc: Array[Byte] = getFunctionBytes(withdrawId, publicFuncType, nonReturnType, withdrawDataType, withdrawBlacklistOpcs)
  val withdrawBlacklistFuncBytes: Array[Byte] = textualFunc("withdraw", Seq(), withdrawBlacklistPara)

  // TotalSupply
  lazy val totalSupplyFunc: Array[Byte] = ContractPermitted.totalSupplyFunc
  val totalSupplyFuncBytes: Array[Byte] = ContractPermitted.totalSupplyFuncBytes

  // MaxSupply
  lazy val maxSupplyFunc: Array[Byte] = ContractPermitted.maxSupplyFunc
  val maxSupplyFuncBytes: Array[Byte] = ContractPermitted.maxSupplyFuncBytes

  // BalanceOf
  lazy val balanceOfFunc: Array[Byte] = ContractPermitted.balanceOfFunc
  val balanceOfFuncBytes: Array[Byte] = ContractPermitted.balanceOfFuncBytes

  // GetIssuer
  lazy val getIssuerFunc: Array[Byte] = ContractPermitted.getIssuerFunc
  val getIssuerFuncBytes: Array[Byte] = ContractPermitted.getIssuerFuncBytes


}
