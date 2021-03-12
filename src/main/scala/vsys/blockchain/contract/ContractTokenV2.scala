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


}
