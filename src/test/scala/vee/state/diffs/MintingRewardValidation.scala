package vee.state.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.Shrink
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.ValidationError
import vee.transaction.MintingTransaction

class MintingRewardValidation extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  property("disallows invalid minting reward") {
    forAll(for {
      recipient <- mintingAddressGen
      amount <- mintingAmountGen
      timestamp <- timestampGen
      currentBlockHeight: Int <- positiveIntGen
    } yield (recipient, amount, timestamp, currentBlockHeight)) { case (recipient, amount, timestamp, currentBlockHeight) =>
      MintingTransaction.create(recipient, amount + 1, timestamp, currentBlockHeight) shouldEqual(Left(ValidationError.WrongMintingReward(amount + 1)))
    }
  }

}
