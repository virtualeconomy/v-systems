package vsys.blockchain

import vsys.utils.base58Length
import vsys.utils.crypto.hash.FastCryptographicHash

package object transaction {

  type AssetId = vsys.blockchain.state.ByteStr
  val AssetIdLength = FastCryptographicHash.DigestSize
  val AssetIdStringLength = base58Length(AssetIdLength)

}
