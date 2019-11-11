package vsys.events

import akka.actor.{Actor, ActorRef, Terminated}
import vsys.utils.{ScorexLogging, SimpleEventQueue}

case class EventWriterActor(queue: SimpleEventQueue, dispatcher: ActorRef) extends Actor with ScorexLogging {
  context.watch(dispatcher)
  
  // TO DO: Should have MQ and protect memory usage in JVM
  // It should handle more events
  def receive = {
    case e: BlockAppendedEvent =>
      queue.enqueue(e)
      dispatcher ! Tuple2(queue, e.maxSize)

    case TxConfirmedEvent(url, scKey, enKey, maxSize, subscribeData) =>
      println(subscribeData)

    case BlockRollbackEvent(url, scKey, enKey, maxSize, subscribeData) =>
      print(subscribeData)

    case Terminated(dispatcher) =>
      log.warn("Event dispatcher actor is stopped. Writer actor is terminating")
  }
}