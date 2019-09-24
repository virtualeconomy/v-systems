package vsys.utils

import scala.concurrent.duration.FiniteDuration
import scala.util.DynamicVariable

class TestTime(val initialT: Long = System.currentTimeMillis()*1000000L+System.nanoTime()%1000000L) extends Time {

  val t = new DynamicVariable(initialT)

  def setTime(tt: Long): this.type = {
    t value_= tt
    this
  }

  def advance(d: FiniteDuration): this.type = {
    t.value_=(t.value + d.toNanos)
    this
  }

  override def correctedTime(): Long = t.value

  override def getTimestamp(): Long = {
    t.value_=(t.value + 1)
    t.value
  }
}
