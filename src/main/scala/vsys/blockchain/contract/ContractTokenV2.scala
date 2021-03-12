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

  private def whitelistCheck(startIndex: Int, sender: Byte, recipient: Byte): Seq[Array[Byte]] = {
    val isSenderInListIndex: Byte = startIndex.toByte
    val isRecipientInListIndex: Byte = (startIndex + 1).toByte
    Seq(
      cdbvrMapGetOrDefault ++ Array(listMap.index, sender, isSenderInListIndex),
      assertTrue ++ Array(isSenderInListIndex),
      cdbvrMapGetOrDefault ++ Array(listMap.index, recipient, isRecipientInListIndex),
      assertTrue ++ Array(isRecipientInListIndex),
    )
  }

  private def blacklistCheck(startIndex: Int, sender: Byte, recipient: Byte): Seq[Array[Byte]] = {
    val valueFalseIndex: Byte = startIndex.toByte
    val isSenderInListIndex: Byte = (startIndex + 1).toByte
    val isRecipientInListIndex: Byte = (startIndex + 2).toByte
    Seq(
      basicConstantGet ++ DataEntry(Array(0.toByte), DataType.Boolean).bytes ++ Array(valueFalseIndex),
      cdbvrMapGetOrDefault ++ Array(listMap.index, sender, isSenderInListIndex),
      assertEqual ++ Array(isSenderInListIndex, valueFalseIndex),
      cdbvrMapGetOrDefault ++ Array(listMap.index, recipient, isRecipientInListIndex),
      assertEqual ++ Array(isRecipientInListIndex, valueFalseIndex),
    )
  }

}
