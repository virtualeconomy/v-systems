package vsys.blockchain.transaction

import vsys.blockchain.state.ByteStr
import play.api.libs.json._
import vsys.utils.crypto.hash.FastCryptographicHash
import vsys.blockchain.transaction.proof.Proofs

trait ProvenTransaction extends Transaction with Signed {

  def toSign: Array[Byte]

  val proofs: Proofs

  override lazy val id: ByteStr = ByteStr(FastCryptographicHash(toSign))

  protected def jsonBase(): JsObject =
    Json.obj(
      "type" -> transactionType.id,
      "id" -> id.base58,
      "fee" -> assetFee._2,
      "timestamp" -> timestamp,
      "proofs" -> proofs.json
    )

  lazy val signatureValid : Boolean = Proofs.verifyProofs(toSign, proofs)
}
