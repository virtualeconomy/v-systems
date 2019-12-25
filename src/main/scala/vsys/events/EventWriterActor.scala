package vsys.events

import akka.actor.{Actor, ActorRef}
import vsys.utils.{ScorexLogging, SimpleEventQueue}

case class EventWriterActor(queue: SimpleEventQueue, dispatcher: ActorRef) extends Actor with ScorexLogging {
  context.watch(dispatcher)
  
  // TO DO: Should have MQ and protect memory usage in JVM
  // It should handle more events
  def receive = {
    case e: Event =>
      queue.enqueue(e)
      dispatcher ! Tuple2(queue, e.maxSize)
  }
}