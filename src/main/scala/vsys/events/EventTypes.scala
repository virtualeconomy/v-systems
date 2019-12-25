package vsys.events

import play.api.libs.json.JsObject

case class Event (
  val url: String,
  val scKey: Option[String],
  val enKey: Option[String],
  val maxSize: Int,
  val subscribeData: JsObject
)