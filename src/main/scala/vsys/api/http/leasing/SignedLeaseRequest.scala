package vsys.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import vsys.account.{AddressOrAlias, PublicKeyAccount}
import vsys.api.http.BroadcastRequest
import vsys.blockchain.transaction.TransactionParser.SignatureStringLength
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.lease.LeaseTransaction

case class SignedLeaseRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              senderPublicKey: String,
                              @ApiModelProperty(required = true)
                              amount: Long,
                              @ApiModelProperty(required = true)
                              fee: Long,
                              @ApiModelProperty(required = true)
                              feeScale: Short,
                              @ApiModelProperty(value = "Recipient address", required = true)
                              recipient: String,
                              @ApiModelProperty(required = true)
                              timestamp: Long,
                              @ApiModelProperty(required = true)
                              signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _recipient <- AddressOrAlias.fromString(recipient)
    _t <- LeaseTransaction.create(_sender, amount, fee, feeScale, timestamp, _recipient, _signature)
  } yield _t
}

object SignedLeaseRequest {
  implicit val broadcastLeaseRequestReadsFormat: Format[SignedLeaseRequest] = Json.format
}
