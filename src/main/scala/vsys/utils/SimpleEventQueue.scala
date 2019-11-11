package vsys.utils

import java.util.concurrent.ConcurrentLinkedQueue
import vsys.events.Event

class SimpleEventQueue() {
  private val queue = new ConcurrentLinkedQueue[Event]()

  def enqueue(x: Event) { queue.offer(x) }

  def multiPush(x: Seq[Event]) { x.map(queue.add(_)) }

  def dequeue(): Event = { queue.poll }

  def size: Int = queue.size

  def dequeAll(): List[Event] = {
    if (queue.size > 0) {
      val curList = queue.toArray.toList.asInstanceOf[List[Event]]
      (1 to curList.size).foreach(x => queue.poll)
      curList
    } else {
      List.empty
    }
  }
}