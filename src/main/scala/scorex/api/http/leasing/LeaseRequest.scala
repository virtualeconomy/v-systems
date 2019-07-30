package scorex.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class LeaseRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                        sender: String,
                        @ApiModelProperty(required = true)
                        amount: Long,
                        @ApiModelProperty(required = true)
                        fee: Long,
                        @ApiModelProperty(required = true)
                        feeScale: Short,
                        @ApiModelProperty(value = "Recipient address", required = true)
                        recipient: String)

object LeaseRequest {
  val leaseRequestReads: Reads[LeaseRequest] = (
      (__ \ "sender").read[String] and
      ((__ \ "amount").read[Long] | (__ \ "amount").read[String].map(_.toLong)) and
      (__ \ "fee").read[Long] and
      (__ \ "feeScale").read[Short] and
      (__ \ "recipient").read[String]
    )(LeaseRequest.apply _)

  val leaseRequestWrites: Writes[LeaseRequest] = (
      (__ \ "sender").write[String] and
      (__ \ "amount").write[Long] and
      (__ \ "fee").write[Long] and
      (__ \ "feeScale").write[Short] and
      (__ \ "recipient").write[String]
    )(unlift(LeaseRequest.unapply))

  implicit val leaseRequestFormat: Format[LeaseRequest] = Format(leaseRequestReads, leaseRequestWrites)
}