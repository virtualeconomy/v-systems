package vsys.api.http.spos

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import vsys.account.PublicKeyAccount
import vsys.api.http.BroadcastRequest
import vsys.blockchain.transaction.TransactionParser.SignatureStringLength
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.spos.ContendSlotsTransaction

case class SignedContendSlotsRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
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
  def toTx: Either[ValidationError, ContendSlotsTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _t <- ContendSlotsTransaction.create(_sender, slotId, fee, feeScale, timestamp, _signature)
  } yield _t
}

object SignedContendSlotsRequest {
  implicit val broadcastContendSlotsRequestReadsFormat: Format[SignedContendSlotsRequest] = Json.format
}
