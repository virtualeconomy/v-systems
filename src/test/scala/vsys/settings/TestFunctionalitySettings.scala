package vsys.settings

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(
    numOfSlots = 60, // easy to test the release case later
    mintingSpeed = 1,
    allowContractTransactionAfterHeight = 0,
    allowDepositWithdrawContractAfterHeight = 0,
    allowExchangeContractAfterHeight = 0
  )

  val ContractDisabled = FunctionalitySettings(
    numOfSlots = 60, // easy to test the release case later
    mintingSpeed = 1,
    allowContractTransactionAfterHeight = 2,
    allowDepositWithdrawContractAfterHeight = 3,
    allowExchangeContractAfterHeight = 4
  )
}