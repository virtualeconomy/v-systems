package com.wavesplatform.settings

import com.wavesplatform.Version
import scorex.utils.ScorexLogging

/**
  * System constants here.
  */

object Constants extends ScorexLogging {
  val ApplicationName = "vee"
  val AgentName = s"VEE v${Version.VersionString}"

  val UnitsInWave = 100000000L
  val TotalWaves = 10000000000L
}
