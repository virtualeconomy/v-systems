package vee.transaction


import play.api.libs.json.{JsObject, Json}
import scorex.serialization.{BytesSerializable, JsonSerializable}
import com.google.common.primitives.{Bytes, Longs}
import scorex.transaction.Transaction

case class ProcessedTransaction(
  status: TransactionStatus.Value,
  feeCharged: Long,
  transaction: Transaction
) extends BytesSerializable with JsonSerializable {

  override lazy val json: JsObject =
    transaction.json ++
    Json.obj(
      "status" -> status.toString,
      "feeCharged" -> feeCharged
    )

  override lazy val bytes: Array[Byte] = Bytes.concat(
    Array(status.id.toByte), Longs.toByteArray(feeCharged), transaction.bytes)

  override def toString: String = json.toString()
}


