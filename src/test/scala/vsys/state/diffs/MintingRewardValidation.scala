package vsys.blockchain.state.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import vsys.blockchain.state.diffs.TransactionDiffer.TransactionValidationError
import vsys.blockchain.transaction.MintingTransaction
import vsys.blockchain.state.diffs.assertDiffEi
import scorex.lagonaki.mocks.TestBlock
import vsys.blockchain.transaction.ValidationError.WrongMintingReward
import vsys.blockchain.consensus.SPoSCalc._

class MintingRewardValidation extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  implicit class ConditionalAssert(v: Either[_, _]) {

    def shouldBeRightIf(cond: Boolean): Assertion = {
      if (cond) {
        v shouldBe an[Right[_, _]]
      } else {
        v shouldBe an[Left[_, _]]
      }
    }
  }

  def validMintingReward(amount: Long): Boolean = {
    amount == MintingReward
  }

  property("disallows create minting transaction with invalid minting reward") {
    forAll(for {
      recipient <- mintingAddressGen
      amount <- positiveLongGen
      timestamp <- timestampGen
      currentBlockHeight: Int <- positiveIntGen
    } yield (recipient, amount, timestamp, currentBlockHeight)) { case (recipient, amount, timestamp, currentBlockHeight) =>
      MintingTransaction.create(recipient, amount, timestamp, currentBlockHeight) shouldBeRightIf validMintingReward(amount)
    }
  }

  /*
This part test broadcast minting transaction cases. When a bad node broadcast an invalid minting transaction to others,
the invalid minting reward will be detected in TransactionDiff instead of transaction create
 */

  val preMinting: Gen[(MintingTransaction, Long)] = for {
    recipient <- mintingAddressGen
    amount <- positiveLongGen
    timestamp <- timestampGen
    // set current height to 2 to pass the validation
    minting: MintingTransaction = MintingTransaction(recipient, amount, timestamp, 2)
  } yield (minting, amount)

  property("disallows to receive minting transaction with invalid minting reward") {
    forAll(preMinting retryUntil(_._2 != MintingReward)) {
      case (minting, amount) =>
        assertDiffEi(Seq(TestBlock.create(Seq())), TestBlock.create(Seq(minting))) { totalDiffEi =>
          totalDiffEi shouldBe Left(TransactionValidationError(WrongMintingReward(amount), minting))
        }
    }
  }

}
