package vee.spos

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import scorex.account.{Address, AddressScheme}
import scorex.utils.ScorexLogging

object SPoSCalc extends ScorexLogging {

  // useful constant
  val MinimalEffectiveBalanceForContender: Long = 100000000000000L

  // update plan: 4 -> 15 slots, 2 -> 30 slots, 1 -> 60 slots
  val SlotGap = AddressScheme.current.chainId match {
    case m if m == 'M'.toByte => 4
    case t if t == 'T'.toByte => 1
    case _ => 1
  }
  // update plan: 15 slots -> 36 vee coins, 30 slots -> 18 vee coins, 60 slots -> 9 vee coins
  val MintingReward = AddressScheme.current.chainId match {
    case m if m == 'M'.toByte => 3600000000L
    case t if t == 'T'.toByte => 900000000L
    case _ => 900000000L
  }

  def weightedBalaceCalc(heightDiff: Int, lastEffectiveBalance: Long, lastWeightedBalance: Long, cntEffectiveBalance: Long, fs: FunctionalitySettings): Long = {
    // mintingSpeed should be larger than 0
    val maxUpdateBlocks = 24 * 60 * 60 / math.max(fs.mintingSpeed, 1) * 1L
    val weightedBalance = math.min(lastEffectiveBalance/maxUpdateBlocks * math.min(maxUpdateBlocks, heightDiff)
      + lastWeightedBalance/maxUpdateBlocks * (maxUpdateBlocks - math.min(maxUpdateBlocks, heightDiff)),
      cntEffectiveBalance)
    weightedBalance
  }

  def mintingBalance(state: StateReader, fs: FunctionalitySettings, account: Address, atHeight: Int): Long = {
    //TODO: we should set the mintingBalance for Genesis case
    // this function only useful for spos minting process
    // here atHeight should be larger than lastHeight (validation)

    val lastHeight = state.lastUpdateHeight(account).getOrElse(0)
    val lastWeightedBalance = state.lastUpdateWeightedBalance(account).getOrElse(0L)
    val lastEffectiveBalance = state.effectiveBalanceAtHeightWithConfirmations(account,lastHeight,0)
    val cntEffectiveBalance = state.effectiveBalance(account)

    val weightedBalance = lastHeight == atHeight match {
      case true => state.lastUpdateWeightedBalance(account).getOrElse(0L)
      case _ => weightedBalaceCalc(atHeight - lastHeight, lastEffectiveBalance, lastWeightedBalance, cntEffectiveBalance, fs)
    }
    weightedBalance
  }

  // TODO: all SPoS related functions will be defined here

}
