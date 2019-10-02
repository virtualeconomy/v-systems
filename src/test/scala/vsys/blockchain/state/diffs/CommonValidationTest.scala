package vsys.blockchain.state.diffs

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.state.EitherExt2
import vsys.blockchain.transaction.{GenesisTransaction, PaymentTransaction, TransactionGen}

class CommonValidationTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("disallows double spending") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, PaymentTransaction)] = for {
      master <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).explicitGet()
      transfer: PaymentTransaction <- paymentGeneratorP(master, recipient)
    } yield (genesis, transfer)

    forAll(preconditionsAndPayment) { case ((genesis, transfer)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, transfer))), TestBlock.create(Seq(transfer))) { blockDiffEi =>
        blockDiffEi should produce("Tx with such id already present")
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer, transfer))) { blockDiffEi =>
        blockDiffEi should produce("Tx with such id already present")
      }
    }
  }
}
