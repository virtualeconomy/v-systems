package vsys.state.diffs

import cats.Monoid
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import vsys.transaction.contract.{ChangeContractStatusAction, ChangeContractStatusTransaction, CreateContractTransaction}
import com.wavesplatform.state2.diffs.{produce, _}
import vsys.contract.Contract
import vsys.transaction.proof.EllipticCurve25519Proof

class ContractTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionsAndContract: Gen[(GenesisTransaction, CreateContractTransaction, CreateContractTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    name <- validAliasStringGen
    content1 <- contractContentGen
    content2 <- contractContentGen
    contract1 = Contract.buildContract(content1, name, true).right.get
    contract2 = Contract.buildContract(content2, name, true).right.get
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create1: CreateContractTransaction = CreateContractTransaction.create(master, contract1, fee, feeScale, ts + 1).right.get
    create2: CreateContractTransaction = CreateContractTransaction.create(master, contract2, fee, feeScale, ts + 2).right.get
  } yield (genesis, create1, create2, create1.fee)

  property("create contract transaction doesn't break invariant") {
    forAll(preconditionsAndContract) { case (genesis, create, _, feeCreate) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(create))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -feeCreate
        totalPortfolioDiff.effectiveBalance shouldBe -feeCreate
        newState.accountTransactionIds(EllipticCurve25519Proof.fromBytes(create.proofs.proofs.head.bytes.arr).toOption.get.publicKey, 2, 0).size shouldBe 2 // genesis and create
      }
    }
  }

  property("create contract transaction fail, when contract name exist") {
    forAll(preconditionsAndContract) { case (genesis, create1, create2, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(create1, create2))) { blockDiffEi =>
        blockDiffEi should produce("Tx with such id already present")
      }
    }
  }

  val preconditionsAndChangeContract: Gen[(GenesisTransaction, GenesisTransaction, CreateContractTransaction, ChangeContractStatusTransaction,
    ChangeContractStatusTransaction, ChangeContractStatusTransaction, ChangeContractStatusTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    name <- validAliasStringGen
    content <- contractContentGen
    contract = Contract.buildContract(content, name, true).right.get
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    changer <- accountGen
    genesis1: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    genesis2: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create: CreateContractTransaction = CreateContractTransaction.create(master, contract, fee, feeScale, ts + 1).right.get
    change1: ChangeContractStatusTransaction = ChangeContractStatusTransaction.create(master, name, ChangeContractStatusAction.Disable, fee, feeScale, ts + 2).right.get
    change2: ChangeContractStatusTransaction = ChangeContractStatusTransaction.create(changer, name, ChangeContractStatusAction.Disable, fee, feeScale, ts + 3).right.get
    change3: ChangeContractStatusTransaction = ChangeContractStatusTransaction.create(master, name, ChangeContractStatusAction.Enable, fee, feeScale, ts + 4).right.get
    change4: ChangeContractStatusTransaction = ChangeContractStatusTransaction.create(master, name, ChangeContractStatusAction.Disable, fee, feeScale, ts + 5).right.get
  } yield (genesis1, genesis2, create, change1, change2, change3, change4, change1.fee)

  property("change contract status transaction doesn't break invariant") {
    forAll(preconditionsAndChangeContract) { case (genesis, _, create, change, _, _, _, feeChange) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, create))), TestBlock.create(Seq(change))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -feeChange
        totalPortfolioDiff.effectiveBalance shouldBe -feeChange
        newState.accountTransactionIds(EllipticCurve25519Proof.fromBytes(change.proofs.proofs.head.bytes.arr).toOption.get.publicKey, 3, 0).size shouldBe 3 // genesis and create, change
      }
    }
  }

  property("change contract status transaction fail, when changer and creator mismatch") {
    forAll(preconditionsAndChangeContract) { case (genesis, _, create, _, change, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, create))), TestBlock.create(Seq(change))) { blockDiffEi =>
        blockDiffEi should produce("Only the creator of the contract can change contract status")
      }
    }
  }

  property("change contract status to enable fail, when contract already enabled") {
    forAll(preconditionsAndChangeContract) { case (genesis, _, create, _, _, change, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, create))), TestBlock.create(Seq(change))) { blockDiffEi =>
        blockDiffEi should produce("The contract already enabled")
      }
    }
  }

  property("change contract status to disable fail, when contract already disabled") {
    forAll(preconditionsAndChangeContract) { case (genesis, _, create, change1, _, _, change2, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, create, change1))), TestBlock.create(Seq(change2))) { blockDiffEi =>
        blockDiffEi should produce("The contract already disabled")
      }
    }
  }

}
