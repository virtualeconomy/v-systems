package vsys.api.http.contract

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json

class ContractRequestsTests extends FunSuite with Matchers {

  test("RegisterContractRequest") {
    val json =
      """
        {
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "fee": 20000000,
          "feeScale": 100,
          "contract": "vsys test",
          "initData": "vsys test dataStack",
          "description": "vsys test description"
        }
      """

    val req = Json.parse(json).validate[RegisterContractRequest].get

    req shouldBe RegisterContractRequest("3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", "vsys test", "vsys test dataStack", Option("vsys test description"), 20000000L, 100)
  }

  test("ExecuteContractFunctionRequest") {
    val json =
      """
        {
          "sender": "3Myss6gmMckKYtka3cKCM563TBJofnxvfD7",
          "fee": 10000000,
          "feeScale": 100,
          "contractId": "vsys test id",
          "functionIndex": 0,
          "functionData": "vsys test dataStack",
          "attachment": "vsys test description"
        }
      """

    val req = Json.parse(json).validate[ExecuteContractFunctionRequest].get

    req shouldBe ExecuteContractFunctionRequest("3Myss6gmMckKYtka3cKCM563TBJofnxvfD7", "vsys test id", 0, "vsys test dataStack", Option("vsys test description"), 10000000L, 100)
  }

  test("SignedRegisterContractRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "fee": 20000000,
         "feeScale": 100,
         "contract": "vsys test",
         "initData": "vsys test dataStack",
         "description": "vsys test description",
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedRegisterContractRequest].get

    req shouldBe SignedRegisterContractRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw", "vsys test", "vsys test dataStack", Option("vsys test description"), 20000000L,
      100, 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }

  test("SignedExecuteContractFunctionRequest") {
    val json =
      """
        {
         "senderPublicKey":"CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
         "fee": 10000000,
         "feeScale": 100,
         "contractId": "vsys test id",
         "functionIndex": 0,
         "functionData": "vsys test dataStack",
         "attachment": "vsys test description",
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedExecuteContractFunctionRequest].get

    req shouldBe SignedExecuteContractFunctionRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
      "vsys test id", 0, "vsys test dataStack", Option("vsys test description"),
      10000000L, 100, 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
  }
}
