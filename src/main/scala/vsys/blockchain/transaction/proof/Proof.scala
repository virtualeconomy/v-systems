package vsys.blockchain.transaction.proof

import vsys.blockchain.state.ByteStr
import play.api.libs.json.JsObject

trait Proof {

  val bytes: ByteStr
  val proofType: ProofType.Value
  val json: JsObject

}

