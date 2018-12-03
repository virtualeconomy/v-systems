package vsys.transaction.api.http.spos

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import vsys.api.http.spos
import vsys.api.http.spos.{ContendSlotsRequest, ReleaseSlotsRequest, SignedContendSlotsRequest, SignedReleaseSlotsRequest}

class SPOSRequestsTests extends FunSuite with Matchers {

  test("ContendSlotsRequest") {
    val json =
      """
        {
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "fee": 100000000000,
          "feeScale": 100,
          "slotId": 0
        }
      """

    val req = Json.parse(json).validate[ContendSlotsRequest].get

    req shouldBe ContendSlotsRequest("3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", 0, 100000000000L, 100)
  }

  test("ReleaseSlotsRequest") {
    val json =
      """
        {
          "sender": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "fee": 100000000000,
          "feeScale": 100,
          "slotId": 0
        }
      """

    val req = Json.parse(json).validate[ReleaseSlotsRequest].get

    req shouldBe ReleaseSlotsRequest("3Myss6gmMckKYtka3cKCM563TBJofnxvfD7", 0, 100000000000L, 100)
  }

  test("SignedContendSlotsRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "slotId": 0,
         "fee":100000000000,
         "feeScale": 100,
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedContendSlotsRequest].get

    req shouldBe spos.SignedContendSlotsRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",100000000000L, 100, 0,
      0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }

  test("SignedReleaseSlotsRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "slotId": 0,
         "timestamp":0,
         "fee": 100000000000,
         "feeScale": 100,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedReleaseSlotsRequest].get

    req shouldBe spos.SignedReleaseSlotsRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
      100000000000L, 100, 0, 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }
}
