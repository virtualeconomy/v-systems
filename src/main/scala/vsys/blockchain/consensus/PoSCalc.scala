package vsys.blockchain.consensus

import vsys.account.Address
import vsys.blockchain.state.reader.StateReader
import vsys.settings.FunctionalitySettings
import vsys.utils.ScorexLogging

object PoSCalc extends ScorexLogging {
  def generatingBalance(state: StateReader, fs: FunctionalitySettings, account: Address, atHeight: Int): Long = {
    state.effectiveBalanceAtHeightWithConfirmations(account, atHeight, 1000)
  }
}
