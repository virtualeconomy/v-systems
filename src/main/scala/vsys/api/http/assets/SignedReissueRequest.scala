package vsys.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import vsys.account.PublicKeyAccount
import vsys.api.http.BroadcastRequest
import vsys.blockchain.transaction.assets.ReissueTransaction
import vsys.blockchain.transaction.TransactionParser.SignatureStringLength
import vsys.blockchain.transaction.{AssetIdStringLength, ValidationError}

object SignedReissueRequest {
  implicit val assetReissueRequestReads: Format[SignedReissueRequest] = Json.format
}

case class SignedReissueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                               senderPublicKey: String,
                                @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                               assetId: String,
                                @ApiModelProperty(required = true, example = "1000000")
                               quantity: Long,
                                @ApiModelProperty(required = true)
                               reissuable: Boolean,
                                @ApiModelProperty(required = true)
                               fee: Long,
                                @ApiModelProperty(required = true)
                               timestamp: Long,
                                @ApiModelProperty(required = true)
                               signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, ReissueTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
    _t <- ReissueTransaction.create(_sender, _assetId, quantity, reissuable, fee, timestamp, _signature)
  } yield _t
}
