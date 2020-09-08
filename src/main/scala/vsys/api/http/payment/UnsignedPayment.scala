package vsys.api.http.payment

import play.api.libs.json.{Format, Json}

case class UnsignedPayment(
    timestamp: Long,
    amount: Long,
    fee: Long,
    feeScale: Short,
    recipient: String,
    senderWalletSeed: String,
    senderAddressNonce: Int)

object UnsignedPayment {
  implicit val paymentReads: Format[UnsignedPayment] = Json.format
}
