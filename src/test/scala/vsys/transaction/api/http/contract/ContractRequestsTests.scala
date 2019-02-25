package vsys.transaction.api.http.contract

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import vsys.api.http.contract
import vsys.api.http.contract._

class ContractRequestsTests extends FunSuite with Matchers {

  test("CreateContractRequest") {
    val json =
      """
        {
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "fee": 20000000,
          "feeScale": 100,
          "contract": "vsys test",
          "dataStack": "vsys test dataStack",
          "description": "vsys test description"
        }
      """

    val req = Json.parse(json).validate[RegisterContractRequest].get

    req shouldBe contract.RegisterContractRequest("3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb", "vsys test", "vsys test dataStack", Option("vsys test description"), 20000000L, 100)
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
         "contract": "vsys test",
         "dataStack": "vsys test dataStack",
         "description": "vsys test description",
         "timestamp":0,
         "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
         }
      """

    val req = Json.parse(json).validate[SignedRegisterContractRequest].get

    req shouldBe contract.SignedRegisterContractRequest("CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw", 20000000L,
      100, "vsys test", "vsys test dataStack", Option("vsys test description"), 0L, "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC")
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
