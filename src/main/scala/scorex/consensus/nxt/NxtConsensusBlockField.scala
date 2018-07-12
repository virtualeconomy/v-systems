package scorex.consensus.nxt

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.block.BlockField
import scorex.crypto.encode.Base58


case class NxtConsensusBlockField(override val value: NxtLikeConsensusBlockData)
  extends BlockField[NxtLikeConsensusBlockData] {

  override val name: String = "nxt-consensus"

  override def bytes: Array[Byte] =
    Bytes.ensureCapacity(Longs.toByteArray(value.mintTime), 8, 0) ++
      Bytes.ensureCapacity(Longs.toByteArray(value.mintBalance), 8, 0) ++
      value.generationSignature



  override def json: JsObject = Json.obj(name -> Json.obj(
    "mint-time" -> value.mintTime,
    "generation-signature" -> Base58.encode(value.generationSignature)
  ))
}
