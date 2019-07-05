package vsys.settings

import java.time.Duration

case class MinerSettings(
  enable: Boolean,
  quorum: Int,
  intervalAfterLastBlockThenGenerationIsAllowed: Duration,
  rewardAddress: String)
