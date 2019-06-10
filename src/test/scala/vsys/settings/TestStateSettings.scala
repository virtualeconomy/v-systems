package vsys.settings

import com.wavesplatform.settings.StateSettings

object TestStateSettings {
  val AllOn = StateSettings(
    txTypeAccountTxIds = true
  )
  val AllOff = StateSettings(
    txTypeAccountTxIds = false
  )
}
