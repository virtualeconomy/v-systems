package scorex.settings

import com.wavesplatform.settings.FunctionalitySettings

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(
    allowTemporaryNegativeUntil = 0L, allowInvalidPaymentTransactionsByTimestamp = 0L,
    requireSortedTransactionsAfter = 0L, generationBalanceDepthFrom50To1000AfterHeight = 0L,
    minimalGeneratingBalanceAfter = 0L,
    allowTransactionsFromFutureUntil = Long.MaxValue, allowUnissuedAssetsUntil = 0L,
    allowBurnTransactionAfter = 0L, allowLeaseTransactionAfter = 0L,
    requirePaymentUniqueIdAfter = 0L, allowExchangeTransactionAfter = 0L,
    allowInvalidReissueInSameBlockUntilTimestamp = 0L, allowCreatealiasTransactionAfter = 0L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0L, resetEffectiveBalancesAtHeight = 0,
    allowLeasedBalanceTransferUntil = 0L,
    allowContendSlotsTransactionAfter = 0L,
    allowReleaseSlotsTransactionAfter = 0L,
    numOfSlots = 2, // easy to test the release case later
    mintingSpeed = 1
  )
}