package vsys.blockchain.state.contract.basic

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.basic.BasicContractGen
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof

class RegisterDepositWithdrawContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with BasicContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val simpleDepositWithdrawContract: Gen[Contract] = contractGen(false)

  val preconditionsAndDepositWithdrawContractTest: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- simpleDepositWithdrawContract
    description <- validDescStringGen
    regContract <- registerDepositWithdrawContractGen(master, contract, description, fee + 10000000000L, ts)
    genesis <- genesisBasicGen(master, ts)
  } yield (genesis, regContract, fee + 10000000000L)

  property("register deposit withdraw contract function transactions doesn't break invariant") {
    forAll(preconditionsAndDepositWithdrawContractTest) { case (genesis, reg: RegisterContractTransaction, fee:Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(reg))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val contractId = reg.contractId.bytes
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))

        newState.accountTransactionIds(master, 2, 0)._2.size shouldBe 2 // genesis, reg
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractDepositWithdraw.contract))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
      }
    }
  }

  val depositWithdrawContract: Gen[Contract] = contractGen(true)

  val preconditionsAndDepositWithdrawProductiveContractTest: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- depositWithdrawContract
    description <- validDescStringGen
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initDepositWithdrawDataStackGen(tokenId.arr)
    regContract <- registerDepositWithdrawProductiveContractGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    genesis <- genesisBasicGen(master, ts)
  } yield (genesis, regContract, fee + 10000000000L)

  property("register deposit withdraw productive contract function transactions doesn't break invariant") {
    forAll(preconditionsAndDepositWithdrawProductiveContractTest) { case (genesis, reg: RegisterContractTransaction, fee:Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(reg))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val contractId = reg.contractId.bytes
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val tokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))
        val tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

        newState.accountTransactionIds(master, 2, 0)._2.size shouldBe 2 // genesis, reg
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractDepositWithdrawProductive.contract))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(tokenIdKey) shouldEqual Some(DataEntry(tokenId.arr, DataType.TokenId))
      }
    }
  }

}
