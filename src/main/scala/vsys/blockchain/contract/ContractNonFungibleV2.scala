package vsys.blockchain.contract

import com.google.common.primitives.Ints
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
}
