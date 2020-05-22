package vsys.blockchain.state.diffs

import com.google.common.primitives.Ints
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{Contract, ContractGenHelper, DataEntry}
import vsys.blockchain.contract.channel.PaymentChannelContractGen
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.{GenesisTransaction, PaymentTransaction, TransactionGen}
import vsys.settings.TestFunctionalitySettings

class CommonValidationContractTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with PaymentChannelContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val tContract: Gen[Contract] = tokenContractGen(true)
  val paymentChannelContract: Gen[Contract] = paymentChannelContractGen()

  val preconditionsAndContract: Gen[(GenesisTransaction, PaymentTransaction, RegisterContractTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- tContract
    dataStack: Seq[DataEntry] <- initTokenDataStackGen(1L, 1L, "init")
    description <- validDescStringGen
    genesis <- genesisTokenGen(master, ts)
    recipient <- accountGen
    transfer: PaymentTransaction <- paymentGeneratorP(ts + 1, master, recipient)
    regTokenContract <- registerTokenGen(master, contract, dataStack, description, fee + 10000000000L, ts + 2)
    pContract <- paymentChannelContract
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataForPaymentChannel: Seq[DataEntry] <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    // Register a payment channel that supports VSYS
    regPaymentChannel <- registerPaymentChannelGen(master, pContract, dataForPaymentChannel, description, fee + 10000000000L, ts + 3)
  } yield (genesis, transfer, regTokenContract, regPaymentChannel)

  property("disallows contract related tx before allowed height") {
    forAll(preconditionsAndContract) { case (genesis, _, regToken: RegisterContractTransaction, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(regToken)), TestFunctionalitySettings.ContractDisabled) { blockDiffEi =>
        blockDiffEi should produce("must not appear before height")
      }
    }
  }

  property("disallows deposit withdraw contract related tx before allowed height") {
    forAll(preconditionsAndContract) { case (g1, p, regToken: RegisterContractTransaction, regChannel: RegisterContractTransaction) =>
      assertDiffEi(Seq(TestBlock.create(Seq(g1)), TestBlock.create(Seq(p))), TestBlock.create(Seq(regToken, regChannel)), TestFunctionalitySettings.ContractDisabled) { blockDiffEi =>
        blockDiffEi should produce("deposit withdraw contracts must not appear before height")
      }
    }
  }

}
