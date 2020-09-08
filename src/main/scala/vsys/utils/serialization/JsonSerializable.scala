package vsys.utils.serialization

import play.api.libs.json.JsObject

trait JsonSerializable {

  def json: JsObject
}
