package scorex.transaction

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import scorex.account.Address
import scorex.utils.ScorexLogging

object PoSCalc extends ScorexLogging {

  val MinimalEffectiveBalanceForGenerator: Long = 1000000000000L
  val AvgBlockTimeDepth: Int = 3

  def generatingBalance(state: StateReader, fs: FunctionalitySettings, account: Address, atHeight: Int): Long = {
    val generatingBalanceDepth = if (atHeight >= fs.generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
    state.effectiveBalanceAtHeightWithConfirmations(account, atHeight, generatingBalanceDepth)
  }

}
