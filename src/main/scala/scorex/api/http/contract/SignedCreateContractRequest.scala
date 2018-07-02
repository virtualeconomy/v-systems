package scorex.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.contract.Contract
import scorex.transaction.TransactionParser.SignatureStringLength
import scorex.transaction.ValidationError
import scorex.transaction.contract.CreateContractTransaction

case class SignedCreateContractRequest (@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                        senderPublicKey: String,
                                        @ApiModelProperty(required = true)
                                        fee: Long,
                                        @ApiModelProperty(value = "Name", required = true)
                                        name: String,
                                        @ApiModelProperty(value = "Content", required = true)
                                        content: String,
                                        @ApiModelProperty(required = true)
                                        timestamp: Long,
                                        @ApiModelProperty(required = true)
                                        signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateContractTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _contract <- Contract.buildContract(content, name, true)
    _t <- CreateContractTransaction.create(_sender, _contract, fee, timestamp, _signature)
  } yield _t
}
object SignedCreateContractRequest {
  implicit val broadcastContractRequestReadsFormat: Format[SignedCreateContractRequest] = Json.format
}
