package vsys.settings

object TestStateSettings {
  val AllOn = StateSettings(
    txTypeAccountTxIds = true,
    txContractTxIds = true
  )
  val AllOff = StateSettings(
    txTypeAccountTxIds = false,
    txContractTxIds = false
  )
}
