package scorex.transaction

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import scorex.account.Address
import scorex.utils.ScorexLogging

object PoSCalc extends ScorexLogging {

  val AvgBlockTimeDepth: Int = 3

  def generatingBalance(state: StateReader, fs: FunctionalitySettings, account: Address, atHeight: Int): Long = {
    state.effectiveBalanceAtHeightWithConfirmations(account, atHeight, 1000)
  }

}
