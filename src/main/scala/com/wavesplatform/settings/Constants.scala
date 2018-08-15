package com.wavesplatform.settings

import com.wavesplatform.Version
import scorex.utils.ScorexLogging

/**
  * System constants here.
  */

object Constants extends ScorexLogging {
  val ApplicationName = "vee"
  val AgentName = s"VEE Core v${Version.VersionString}"

  val UnitsInVee = 100000000L
  val TotalVee = 10000000000L
}
