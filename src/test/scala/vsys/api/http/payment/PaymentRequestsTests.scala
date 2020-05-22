package vsys.api.http.payment

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.{JsSuccess, Json}

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

    val req = Json.parse(json).validate[PaymentRequest]

    req shouldEqual JsSuccess(PaymentRequest(100000000000000L, 10000000L, 100, "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", Option("v"), "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV"))
  }

  test("PaymentRequest with string amount") {
    val json =
      """
        {
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "fee": 10000000,
          "feeScale": 100,
          "recipient": "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV",
          "amount": "0100000000000000",
          "attachment": "v"
        }
      """

    val req = Json.parse(json).validate[PaymentRequest]

    req shouldEqual JsSuccess(PaymentRequest(100000000000000L, 10000000L, 100, "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", Option("v"), "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV"))
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

    val req = Json.parse(json).validate[SignedPaymentRequest]

    req shouldBe JsSuccess(SignedPaymentRequest(0L, 100000000000000L, 10000000L, 100, "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV",
      "CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw", Option("v"), "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"))
  }

  test("SignedPaymentRequest with string amount") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "fee": 10000000,
         "feeScale": 100,
         "recipient": "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV",
         "amount": "100000000000000",
         "attachment": "v",
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedPaymentRequest]

    req shouldBe JsSuccess(SignedPaymentRequest(0L, 100000000000000L, 10000000L, 100, "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV",
      "CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw", Option("v"), "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"))
  }

}
