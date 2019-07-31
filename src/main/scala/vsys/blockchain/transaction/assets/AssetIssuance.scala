package vsys.blockchain.transaction.assets

import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.SignedTransaction

/*
  Issue or Reissue of Asset
 */
trait AssetIssuance extends SignedTransaction {
  val assetId: ByteStr
  val reissuable: Boolean
  val quantity: Long
}
