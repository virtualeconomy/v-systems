package vsys.settings

object TestStateSettings {
  val AllOn = StateSettings(
    txTypeAccountTxIds = true
  )
  val AllOff = StateSettings(
    txTypeAccountTxIds = false
  )
}
