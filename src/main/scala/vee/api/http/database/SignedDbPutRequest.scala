package vee.api.http.database

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.TransactionParser.SignatureStringLength
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.DbDataTypeError
import vee.database.{DataType, Entry}
import vee.transaction.database.DbPutTransaction

case class SignedDbPutRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                     senderPublicKey: String,
                                     @ApiModelProperty(value = "name", required = true)
                                     dbKey: String,
                                     @ApiModelProperty(value = "dataType", required = true)
                                     dataType: String,
                                     @ApiModelProperty(value = "data")
                                     data: String,
                                     @ApiModelProperty(required = true)
                                     fee: Long,
                                     @ApiModelProperty(value = "Fee Scale (default 100)", required = true)
                                     feeScale: Short,
                                     @ApiModelProperty(required = true)
                                     timestamp: Long,
                                     @ApiModelProperty(required = true)
                                     signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, DbPutTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _dataType <- DataType.values.find(_.toString == dataType) match {
      case Some(x) => Right(x)
      case None =>Left(DbDataTypeError(dataType))
    }
    dbEntry <- Entry.buildEntry(data, _dataType)
    _t <- DbPutTransaction.create(_sender, dbKey, dbEntry, fee, feeScale, timestamp, _signature)
  } yield _t
}

object SignedDbPutRequest {
  implicit val broadcastDbPutRequestReadsFormat: Format[SignedDbPutRequest] = Json.format
}
