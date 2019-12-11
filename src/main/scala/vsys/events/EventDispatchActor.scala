package vsys.events

import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpEntity, ContentTypes, HttpRequest, HttpMethods}
import akka.http.scaladsl.model.headers.RawHeader
import play.api.libs.json.{JsArray, JsObject}
import scorex.crypto.encode.Base58
import vsys.utils.{ScorexLogging, SimpleEventQueue}
import vsys.utils.crypto.EllipticCurveImpl
import vsys.account.PrivateKeyAccount

case class EventDispatchActor(actorSys: ActorSystem) extends Actor with ScorexLogging {

  def receive = process

  def process: Receive = {
    case (q: SimpleEventQueue, maxSize: Int) =>
      val curSize = q.size
      if (maxSize >= 1 && maxSize <= 3000 && curSize >= maxSize || curSize >= 1000) {
        val curList = q.dequeAll()
        packAndDispatch(curList, maxSize)
      }
  }

  private def packAndDispatch(eventList: List[Event], payloadSize: Int) = {
    val groupedMap = eventList.groupBy { case e: Event => (e.url, e.scKey, e.enKey) }
    for ((key, value) <- groupedMap) {
      val splitted = value.map(_.subscribeData).grouped(payloadSize).toList
      val payload = splitted.map {x: List[JsObject] => x.foldLeft(JsArray())((accum, onePay) => accum :+ onePay)}
      key match {
        case (urlIn: String, scKey: Option[String], enKey: Option[String]) =>
          val requests = payload.map {p =>
            val requestEntity = HttpEntity(ContentTypes.`application/json`, p.toString())
            val request = HttpRequest(
              method = HttpMethods.POST,
              uri = urlIn,
              entity = requestEntity
            )
            scKey match {
              case None => request
              case Some(s) => request.addHeader(RawHeader("secretKey", s))
            }

            enKey match {
              case None => request
              case Some(enKey) => 
                val signature = EllipticCurveImpl.sign(PrivateKeyAccount(enKey.getBytes(StandardCharsets.UTF_8)), p.toString().getBytes(StandardCharsets.UTF_8))
                request.addHeader(RawHeader("payloadSignature", Base58.encode(signature)))
            }
          }
          requests.map(Http(actorSys).singleRequest(_))
        case _ => log.error("Event Dispatch Actor: Invalid")
      }
    }
  }
}