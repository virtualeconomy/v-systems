package vsys.utils

import scala.concurrent.duration.FiniteDuration

class TestTime(var t: Long = System.currentTimeMillis()*1000000L+System.nanoTime()%1000000L) extends Time {
  def setTime(tt: Long): this.type = {
    t = tt
    this
  }

  def advance(d: FiniteDuration): this.type = {
    t += d.toNanos
    this
  }

  override def correctedTime(): Long = t

  override def getTimestamp(): Long = {
    t += 1
    t
  }
}
