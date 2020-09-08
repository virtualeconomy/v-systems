package vsys.api.http.spos

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import vsys.account.PublicKeyAccount
import vsys.api.http.BroadcastRequest
import vsys.blockchain.transaction.TransactionParser.SignatureStringLength
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.spos.ReleaseSlotsTransaction

case class SignedReleaseSlotsRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                    senderPublicKey: String,
                                    @ApiModelProperty(required = true)
                                    fee: Long,
                                    @ApiModelProperty(required = true)
                                    feeScale: Short,
                                    @ApiModelProperty(required = true)
                                    slotId: Int,
                                    @ApiModelProperty(required = true)
                                    timestamp: Long,
                                    @ApiModelProperty(required = true)
                                    signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, ReleaseSlotsTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _t <- ReleaseSlotsTransaction.create(_sender, slotId, fee, feeScale, timestamp, _signature)
  } yield _t
}

object SignedReleaseSlotsRequest {
  implicit val broadcastReleaseSlotsRequestReadsFormat: Format[SignedReleaseSlotsRequest] = Json.format
}
