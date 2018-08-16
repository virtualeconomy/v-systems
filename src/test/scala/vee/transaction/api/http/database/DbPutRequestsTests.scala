package vee.transaction.api.http.database

import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.Json
import vee.api.http.database.{DbPutRequest, SignedDbPutRequest}

class DbPutRequestsTests extends FunSuite with Matchers {

  test("DbPutRequest") {
    val json =
      """
        {
          "sender": "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb",
          "name": "DB Op",
          "data": "some data",
          "dataType": "ByteArray",
          "fee": 100000000,
          "feeScale": 100
        }
      """

    val req = Json.parse(json).validate[DbPutRequest].get

    req.sender shouldBe "3MwKzMxUKaDaS4CXM8KNowCJJUnTSHDFGMb"
    req.name shouldBe "DB Op"
    req.data shouldBe "some data"
    req.dataType shouldBe "ByteArray"
    req.fee shouldBe 100000000L
    req.feeScale shouldBe 100
  }

  test("SignedDbPutRequest") {
    val json =
      """
          {
            "senderPublicKey": "CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw",
            "name": "DB Op",
            "data": "some data",
            "dataType": "ByteArray",
            "fee": 100000000,
            "feeScale": 100,
            "timestamp": 0,
            "signature":"4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
          }
        """

    val req = Json.parse(json).validate[SignedDbPutRequest].get

    req.senderPublicKey shouldBe "CRxqEuxhdZBEHX42MU4FfyJxuHmbDBTaHMhM3Uki7pLw"
    req.name shouldBe "DB Op"
    req.data shouldBe "some data"
    req.dataType shouldBe "ByteArray"
    req.fee shouldBe 100000000L
    req.feeScale shouldBe 100
    req.timestamp shouldBe 0
    req.signature shouldBe "4VPg4piLZGQz3vBqCPbjTfAR4cDErMi57rDvyith5XrQJDLryU2w2JsL3p4ejEqTPpctZ5YekpQwZPTtYiGo5yPC"
  }
}
