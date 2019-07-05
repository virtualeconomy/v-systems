package vsys.settings

import vsys.settings.StateSettings

object TestStateSettings {
  val AllOn = StateSettings(
    txTypeAccountTxIds = true
  )
  val AllOff = StateSettings(
    txTypeAccountTxIds = false
  )
}
