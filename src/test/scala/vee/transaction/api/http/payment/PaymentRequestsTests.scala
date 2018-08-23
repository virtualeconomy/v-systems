package vee.transaction.api.http.payment

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import vee.api.http.vee.SignedPaymentRequest
import scorex.api.http.assets.PaymentRequest

class PaymentRequestsTests extends FunSuite with Matchers {

  test("PaymentRequest") {
    val json =
      """
        {
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "fee": 10000000,
          "feeScale": 100,
          "recipient": "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV",
          "amount": 100000000000000,
          "attachment": "v"
        }
      """

    val req = Json.parse(json).validate[PaymentRequest].get

    req shouldBe PaymentRequest(100000000000000L, 10000000L, 100, "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", Option("v"), "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV")
  }

  test("SignedPaymentRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "fee": 10000000,
         "feeScale": 100,
         "recipient": "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV",
         "amount": 100000000000000,
         "attachment": "v",
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedPaymentRequest].get

    req shouldBe SignedPaymentRequest(0L, 100000000000000L, 10000000L, 100, "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV",
      "CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw", Option("v"), "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }

}
