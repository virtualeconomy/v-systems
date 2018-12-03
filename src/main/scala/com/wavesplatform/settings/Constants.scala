package com.wavesplatform.settings

import com.wavesplatform.Version
import scorex.utils.ScorexLogging

/**
  * System constants here.
  */

object Constants extends ScorexLogging {
  val ApplicationName = "V SYSTEMS"
  val AgentName = s"VSYS Core v${Version.VersionString}"

  val UnitsInVsys = 100000000L
  val TotalVsys = 10000000000L // unuse in mainnet
}
