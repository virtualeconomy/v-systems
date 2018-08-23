package vee.transaction.proof

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.JsObject

trait Proof {

  val bytes: ByteStr
  val proofType: ProofType.Value
  val json: JsObject

}

