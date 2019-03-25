package vsys.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.crypto.encode.Base58
import vsys.contract.{Contract, DataEntry}
import scorex.transaction.TransactionParser.SignatureStringLength
import scorex.transaction.ValidationError
import vsys.transaction.contract.RegisterContractTransaction


case class SignedRegisterContractRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                         senderPublicKey: String,
                                         @ApiModelProperty(value = "Base58 encoded contract", required = true)
                                         contract: String,
                                         @ApiModelProperty(value = "Base58 encoded dataStack", required = true)
                                         dataStack: String,
                                         @ApiModelProperty(value = "Base58 encoded description of contract")
                                         description: Option[String],
                                         @ApiModelProperty(required = true)
                                         fee: Long,
                                         @ApiModelProperty(required = true)
                                         feeScale: Short,
                                         @ApiModelProperty(required = true)
                                         timestamp: Long,
                                         @ApiModelProperty(required = true)
                                         signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, RegisterContractTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _contract <- Contract.fromBase58String(contract)
    _dataStack <- DataEntry.fromBase58String(dataStack)
    _description = description.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray)
    _t <- RegisterContractTransaction.create(_sender, _contract, _dataStack, _description, fee, feeScale, timestamp, _signature)
  } yield _t
}

object SignedRegisterContractRequest {
  implicit val broadcastRegisterContractRequestReadsFormat: Format[SignedRegisterContractRequest] = Json.format
}
