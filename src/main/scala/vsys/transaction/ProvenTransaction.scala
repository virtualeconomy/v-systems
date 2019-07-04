package vsys.transaction

import com.wavesplatform.state2.ByteStr
import play.api.libs.json._
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction._
import vsys.transaction.proof.Proofs

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
