package vsys.settings

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(
    numOfSlots = 60, // easy to test the release case later
    mintingSpeed = 1,
    allowContractTransactionAfterHeight = 0
  )
}