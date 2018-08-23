package vee.transaction.api.http.contract

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import vee.api.http.contract
import vee.api.http.contract.{ChangeContractStatusRequest, CreateContractRequest, SignedChangeContractStatusRequest, SignedCreateContractRequest}

class ContractRequestsTests extends FunSuite with Matchers {

  test("CreateContractRequest") {
    val json =
      """
        {
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "fee": 20000000,
          "feeScale": 100,
          "name": "test",
          "content": "vee test content"
        }
      """

    val req = Json.parse(json).validate[CreateContractRequest].get

    req shouldBe contract.CreateContractRequest("3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", "test", "vee test content", 20000000L, 100)
  }

  test("ChangeContractStatusRequest") {
    val json =
      """
        {
          "sender": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "fee": 10000000,
          "feeScale": 100,
          "contractName": "test"
        }
      """

    val req = Json.parse(json).validate[ChangeContractStatusRequest].get

    req shouldBe contract.ChangeContractStatusRequest("3Myss6gmMckKYtka3cKCM563TBJofnxvfD7", "test", 10000000L, 100)
  }

  test("SignedCreateContractRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "fee": 20000000,
         "feeScale": 100,
         "name": "test",
         "content": "vee test content",
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedCreateContractRequest].get

    req shouldBe contract.SignedCreateContractRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw", 20000000L,
      100, "test", "vee test content", 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }

  test("SignedChangeContractStatusRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "fee": 10000000,
         "feeScale": 100,
         "contractName": "test",
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedChangeContractStatusRequest].get

    req shouldBe contract.SignedChangeContractStatusRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw", "test",
      10000000L, 100, 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }
}
