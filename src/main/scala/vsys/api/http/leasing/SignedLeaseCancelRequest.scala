package vsys.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import vsys.account.PublicKeyAccount
import vsys.api.http.BroadcastRequest
import vsys.blockchain.transaction.TransactionParser.SignatureStringLength
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.lease.LeaseCancelTransaction

case class SignedLeaseCancelRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                    senderPublicKey: String,
                                    @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                    txId: String,
                                    @ApiModelProperty(required = true)
                                    timestamp: Long,
                                    @ApiModelProperty(required = true)
                                    signature: String,
                                    @ApiModelProperty(required = true)
                                    fee: Long,
                                    @ApiModelProperty(required = true)
                                    feeScale: Short) extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseCancelTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _leaseTx <- parseBase58(txId, "invalid.leaseTx", SignatureStringLength)
    _t <- LeaseCancelTransaction.create(
      _sender,
      _leaseTx,
      fee,
      feeScale,
      timestamp,
      _signature)
  } yield _t
}

object SignedLeaseCancelRequest {
  implicit val leaseCancelRequestFormat: Format[SignedLeaseCancelRequest] = Json.format
}

