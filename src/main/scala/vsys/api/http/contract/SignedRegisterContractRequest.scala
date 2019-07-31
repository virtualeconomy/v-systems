package vsys.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import vsys.account.PublicKeyAccount
import vsys.api.http.BroadcastRequest
import vsys.utils.serialization.Deser
import vsys.blockchain.contract.{Contract, DataEntry}
import vsys.blockchain.transaction.TransactionParser.SignatureStringLength
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.contract.RegisterContractTransaction


case class SignedRegisterContractRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                         senderPublicKey: String,
                                         @ApiModelProperty(value = "Base58 encoded contract", required = true)
                                         contract: String,
                                         @ApiModelProperty(value = "Base58 encoded init data", required = true)
                                         initData: String,
                                         @ApiModelProperty(value = "Description of contract")
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
    _initData <- DataEntry.fromBase58String(initData)
    _description = description.filter(_.nonEmpty).getOrElse(Deser.deserilizeString(Array.emptyByteArray))
    _t <- RegisterContractTransaction.create(_sender, _contract, _initData, _description, fee, feeScale, timestamp, _signature)
  } yield _t
}

object SignedRegisterContractRequest {
  implicit val broadcastRegisterContractRequestReadsFormat: Format[SignedRegisterContractRequest] = Json.format
}
