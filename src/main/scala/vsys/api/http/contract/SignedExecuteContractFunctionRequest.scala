package vsys.api.http.contract

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import vsys.account.{ContractAccount, PublicKeyAccount}
import vsys.api.http.BroadcastRequest
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.TransactionParser.SignatureStringLength
import vsys.blockchain.contract.DataEntry
import vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction
import scorex.crypto.encode.Base58


case class SignedExecuteContractFunctionRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                                senderPublicKey: String,
                                                @ApiModelProperty(value = "Base58 encoded contract id", required = true)
                                                contractId: String,
                                                @ApiModelProperty(required = true)
                                                functionIndex: Short,
                                                @ApiModelProperty(value = "Base58 encoded function data", required = true)
                                                functionData: String,
                                                @ApiModelProperty(value = "Base58 encoded attachment")
                                                attachment: Option[String],
                                                @ApiModelProperty(required = true, example = "30000000")
                                                fee: Long,
                                                @ApiModelProperty(required = true, example = "100")
                                                feeScale: Short,
                                                @ApiModelProperty(required = true)
                                                timestamp: Long,
                                                @ApiModelProperty(required = true)
                                                signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, ExecuteContractFunctionTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _contractId <- ContractAccount.fromString(contractId)
    _functionData <- DataEntry.fromBase58String(functionData)
    _attachment = attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray)
    _t <- ExecuteContractFunctionTransaction.create(_sender, _contractId, functionIndex, _functionData, _attachment, fee, feeScale, timestamp, _signature)
  } yield _t
}

object SignedExecuteContractFunctionRequest {
  implicit val broadcastExecuteContractFunctionRequestReadsFormat: Format[SignedExecuteContractFunctionRequest] = Json.format
}
