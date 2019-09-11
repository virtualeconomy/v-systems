package vsys.events

import akka.actor.Actor
import vsys.utils.ScorexLogging
import vsys.account.Address

class EventWriterActor() extends Actor with ScorexLogging {

  // TO DO: Should have MQ and protect memory usage in JVM
  // It should handle more events
  override def receive = {
    case BlockAppendedEvent(url, secretKey, enKey, maxSize, eventData) =>
      eventData.map {aList =>
        aList match {case (_, _, accs: Set[Address]) => accs.map(addr => log.info(addr.toString))}
      }
  }
}