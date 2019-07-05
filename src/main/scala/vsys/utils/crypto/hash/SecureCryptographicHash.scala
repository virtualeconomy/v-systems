package vsys.utils.crypto.hash

import scorex.crypto.hash.CryptographicHash

/**
 * Hash function for cases, where security is more important, then speed
 */
object SecureCryptographicHash extends CryptographicHash {

  private val hf: CryptographicHash = vsys.utils.crypto.HashChain

  override val DigestSize: Int = hf.DigestSize

  override def hash(in: Message): Digest = hf.hash(in)
}
