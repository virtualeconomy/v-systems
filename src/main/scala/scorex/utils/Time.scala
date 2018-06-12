package scorex.utils

import java.net.InetAddress

import org.apache.commons.net.ntp.NTPUDPClient

import scala.util.Try

trait Time {
  def correctedTime(): Long
  def getTimestamp() : Long
}

class TimeImpl extends Time with ScorexLogging {
  //TimeTillUpdate: 10min
  private val TimeTillUpdate = 1000000000L * 60 * 10L
  private val NtpServer = "pool.ntp.org"

  private var lastUpdate = 0L
  private var offset = 0L
  private var cntTime = 0L

  def correctedTime(): Long = {
    //CHECK IF OFFSET NEEDS TO BE UPDATED
    if (System.currentTimeMillis()*1000000L+System.nanoTime()%1000000L > lastUpdate + TimeTillUpdate) {
      Try {
        //update the offset in nanoseconds
        updateOffSet()
        lastUpdate = System.currentTimeMillis()*1000000L+System.nanoTime()%1000000L

        log.info("Adjusting time with " + offset + " nanoseconds.")
      } recover {
        case e: Throwable =>
          log.warn("Unable to get corrected time", e)
      }
    }

    //CALCULATE CORRECTED TIME
    val cnt = System.currentTimeMillis()*1000000L+System.nanoTime()%1000000L + offset
    cntTime = if (cnt<=cntTime && cntTime-cnt<=1000000L) cnt+1000000L else cnt
    cntTime
  }


  private var txTime: Long = 0

  def getTimestamp: Long = {
    //guarantee the cnt_SystemTime > last_Timestamp+1
    txTime = Math.max(correctedTime(), txTime + 1)
    txTime
  }

  private def updateOffSet() {
    val client = new NTPUDPClient()
    //setDefaultTimeout(int), Set the default timeout in milliseconds
    // to use when opening a socket. After a call to open, 
    //the timeout for the socket is set using this value. 
    //so will not change here
    client.setDefaultTimeout(10000)

    try {
      client.open()
      //Retrieves the time information from the specified server on the default NTP 
      //port and returns it. The time is the number of miliiseconds since 00:00 (midnight) 1 January 1900 UTC,
      val info = client.getTime(InetAddress.getByName(NtpServer))
      info.computeDetails()
      //return in milliseconds, change to nanoseconds
      if (Option(info.getOffset).isDefined) offset = info.getOffset*1000000L
    } catch {
      case t: Throwable => log.warn("Problems with NTP: ", t)
    } finally {
      client.close()
    }
  }
}

object NTP extends TimeImpl
