package vsys.blockchain.contract

import com.google.common.primitives.Ints
import vsys.blockchain.contract.ContractGen.{StateMap, StateVar}
import vsys.blockchain.state._
import vsys.utils.serialization.Deser

object ContractNonFungibleV2 {
  lazy val contractNFTWhiteList: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq()
  ).explicitGet()

  lazy val contractNFTBlackList: Contract = Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq()
  ).explicitGet()

  // StateVar
  val stateVarName = List("issuer", "maker")
  val issuerStateVar: StateVar = StateVar(0.toByte, DataType.Address.id.toByte)
  val makerStateVar: StateVar = StateVar(1.toByte, DataType.Address.id.toByte)
  lazy val stateVarTextual: Array[Byte] = Deser.serializeArrays(stateVarName.map(x => Deser.serilizeString(x)))

  // StateMap
  val stateMapWhitelist = List("whitelist", "userAccount", "isInList")
  val stateMapBlacklist = List("blacklist", "userAccount", "isInList")
  val listMap: StateMap = StateMap(0.toByte, DataType.Account.id.toByte, DataType.Boolean.id.toByte)

}
