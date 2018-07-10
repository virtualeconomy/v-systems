package vee.spos

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import scorex.account.Address
import scorex.utils.ScorexLogging

object SPoSCalc extends ScorexLogging {

  // useful constant
  val MinimalEffectiveBalanceForGenerator: Long = 1000000000000L

  def mintingBalance(state: StateReader, fs: FunctionalitySettings, account: Address, atHeight: Int): Long = {
    //TODO: we should set the mintingBalance for Genesis case
    // this function only use for spos minting process
    // here atHeight should larger than lastHeight (validation)

    val lastHeight = state.lastUpdateHeight(account).getOrElse(0)
    val lastWeightedBalance = state.lastUpdateWeightedBalance(account).getOrElse(0L)
    val maxUpdateBlocks = 24 * 60 * 60 / fs.mintingSpeed * 1L
    val lastEffectiveBalance = state.effectiveBalanceAtHeightWithConfirmations(account,lastHeight,0)
    val cntEffectiveBalance = state.effectiveBalance(account)
    val weightedBalance = lastHeight == atHeight match {
      case true => state.lastUpdateWeightedBalance(account).getOrElse(0L)
      case _ => math.min(lastEffectiveBalance/maxUpdateBlocks * math.min(maxUpdateBlocks, atHeight - lastHeight)
        + lastWeightedBalance/maxUpdateBlocks * (maxUpdateBlocks - math.min(maxUpdateBlocks, atHeight - lastHeight)),
        cntEffectiveBalance)
    }
    weightedBalance
  }

  // TODO: all SPoS related functions will be defined here

}
