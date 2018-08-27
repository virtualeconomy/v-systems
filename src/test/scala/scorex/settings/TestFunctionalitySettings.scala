package scorex.settings

import com.wavesplatform.settings.FunctionalitySettings

object TestFunctionalitySettings {
  val Enabled = FunctionalitySettings(
    numOfSlots = 2, // easy to test the release case later
    mintingSpeed = 1
  )
}