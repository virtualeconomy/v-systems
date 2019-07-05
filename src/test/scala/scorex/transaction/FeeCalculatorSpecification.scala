package vsys.blockchain.transaction

import com.typesafe.config.ConfigFactory
import com.wavesplatform.TransactionGen
import vsys.settings.FeesSettings
import vsys.blockchain.state.ByteStr
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import vsys.account.{Address, PrivateKeyAccount}
import vsys.blockchain.transaction.assets._
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.database.DbPutTransaction
import vsys.blockchain.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}


class FeeCalculatorSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
  with Matchers with TransactionGen {


  private val configString =
    """vsys {
      |  fees {
      |    payment {
      |      VSYS = 10000000
      |    }
      |    issue {
      |      VSYS = 100000000
      |    }
      |    transfer {
      |      VSYS = 10000000
      |      "JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN" = 2
      |    }
      |    reissue {
      |      VSYS = 10000000
      |    }
      |    burn {
      |      VSYS = 10000000
      |    }
      |    lease {
      |      VSYS = 10000000
      |    }
      |    lease-cancel {
      |      VSYS = 10000000
      |    }
      |    create-alias {
      |      VSYS = 10000000
      |    }
      |    contend-slots {
      |      VSYS = 5000000000000
      |    }
      |    release-slots {
      |      VSYS = 10000000
      |    }
      |    register-contract {
      |      VSYS = 10000000000
      |    }
      |    execute-contract-function {
      |      VSYS = 30000000
      |    }
      |    db-put {
      |      VSYS = 10000000
      |    }
      |  }
      |}""".stripMargin

  private val config = ConfigFactory.parseString(configString)

  private val mySettings = FeesSettings.fromConfig(config)

  private val WhitelistedAsset = ByteStr.decodeBase58("JAudr64y6YxTgLn9T5giKKqWGkbMfzhdRAxmNNfn6FJN").get

  implicit class ConditionalAssert(v: Either[_, _]) {

    def shouldBeRightIf(cond: Boolean): Assertion = {
      if (cond) {
        v shouldBe an[Right[_, _]]
      } else {
        v shouldBe an[Left[_, _]]
      }
    }
  }

  property("Transfer transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(transferGen) { tx: TransferTransaction =>
      if (tx.feeAssetId.isEmpty) {
        feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
      } else {
        feeCalc.enoughFee(tx) shouldBe an[Left[_,_]]
      }
    }
  }

  property("Transfer transaction with fee in asset") {
    val feeCalculator = new FeeCalculator(mySettings)
    val sender = PrivateKeyAccount(Array.emptyByteArray)
    val recipient = Address.fromString("AU3AorwRqQhYpRUR3ednaWFvuxAjYCNWd26").right.get
    val tx1: TransferTransaction = TransferTransaction.create(Some(WhitelistedAsset), sender, recipient, 1000000, 100000000,
      Some(WhitelistedAsset), 2, Array.emptyByteArray).right.get
    val tx2: TransferTransaction = TransferTransaction.create(Some(WhitelistedAsset), sender, recipient, 1000000, 100000000,
      Some(WhitelistedAsset), 1, Array.emptyByteArray).right.get

    feeCalculator.enoughFee(tx1) shouldBe a[Right[_,_]]
    feeCalculator.enoughFee(tx2) shouldBe a[Left[_,_]]
  }

  property("Payment transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(paymentGen) { tx: PaymentTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
    }
  }

  property("Issue transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(issueGen) { tx: IssueTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000000)
    }
  }

  property("Reissue transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(reissueGen) { tx: ReissueTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
    }
  }

  property("Burn transaction ") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(burnGen) { tx: BurnTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
    }
  }

  property("Lease transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(leaseGen) { tx: LeaseTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
    }
  }

  property("Lease cancel transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
    }
  }

  property("Create alias transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
    }
  }

  property("Contend slots transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(contendSlotsGen) { tx: ContendSlotsTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 5000000000000L)
    }
  }

  property("Release slots transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(releaseSlotsGen) { tx: ReleaseSlotsTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
    }
  }

  property("Register contract transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(registerContractGen) { tx: RegisterContractTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000000000L)
    }
  }

  property("Execute contract function transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(executeContractGen) { tx: ExecuteContractFunctionTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 30000000)
    }
  }

  property("Db put transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(dbPutGen) { tx: DbPutTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 10000000)
    }
  }
}