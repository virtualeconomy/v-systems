package vsys.blockchain.transaction

import com.typesafe.config.ConfigFactory
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Assertion, Matchers, PropSpec}
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.database.DbPutTransaction
import vsys.blockchain.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.settings.FeesSettings

class FeeCalculatorSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks
  with Matchers with TransactionGen {


  private val configString =
    """vsys {
      |  fees {
      |    payment {
      |      VSYS = 10000000
      |    }
      |    lease {
      |      VSYS = 10000000
      |    }
      |    lease-cancel {
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

  implicit class ConditionalAssert(v: Either[_, _]) {

    def shouldBeRightIf(cond: Boolean): Assertion = {
      if (cond) {
        v shouldBe an[Right[_, _]]
      } else {
        v shouldBe an[Left[_, _]]
      }
    }
  }

  property("Payment transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(paymentGen) { tx: PaymentTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.transactionFee >= 10000000)
    }
  }

  property("Lease transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(leaseGen) { tx: LeaseTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.transactionFee >= 10000000)
    }
  }

  property("Lease cancel transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.transactionFee >= 10000000)
    }
  }

  property("Contend slots transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(contendSlotsGen) { tx: ContendSlotsTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.transactionFee >= 5000000000000L)
    }
  }

  property("Release slots transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(releaseSlotsGen) { tx: ReleaseSlotsTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.transactionFee >= 10000000)
    }
  }

  property("Register contract transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(registerContractGen) { tx: RegisterContractTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.transactionFee >= 100000000000L)
    }
  }

  property("Execute contract function transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(executeContractGen) { tx: ExecuteContractFunctionTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.transactionFee >= 30000000)
    }
  }

  property("Db put transaction") {
    val feeCalc = new FeeCalculator(mySettings)
    forAll(dbPutGen) { tx: DbPutTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.transactionFee >= 10000000)
    }
  }
}