package vsys.blockchain.state.contract.lock

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.blockchain.block.TestBlock
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract._
import vsys.blockchain.contract.lock.LockContractGen
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.proof.EllipticCurve25519Proof
import vsys.blockchain.transaction.contract._

class ExecuteLockContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with LockContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val lockContract: Gen[Contract] = lockContractGen()
  val tokenContract: Gen[Contract] = tokenContractGen(false)

  val preconditionsAndLockContractTest: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisLockGen(master, ts)
    contract <- lockContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initLockContractDataStackGen(sysTokenId.arr)
    // register a lock contract only support VSYS
    regContract <- registerLockContractGen(master, contract, dataStack, description, fee + 10000000000L, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    // lock to ts + 2
    lockVSYS <- lockAssetGen(master, contractId, ts + 2, attach, fee, ts + 2)
    withdrawData = Seq(contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(1L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    // withdraw, time based on last block timestamp
    withdrawVSYS <- withdrawVSYSGen(master, withdrawData, withdrawType, attach, fee, ts + 2)
  } yield (genesis, regContract, depositVSYS, lockVSYS, withdrawVSYS, fee)

  property("execute lock contract interact with system contract functions doesn't break invariant") {
    forAll(preconditionsAndLockContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    deposit, lockV: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(lockV.timestamp, Seq(reg, deposit, lockV))),
        TestBlock.create(withdraw.timestamp + 1, Seq(withdraw))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        val master = EllipticCurve25519Proof.fromBytes(reg.proofs.proofs.head.bytes.arr).explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val contractId = reg.contractId.bytes
        val VSYSId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val contractTokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        val masterBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val masterLockTimeInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte), DataEntry(masterBytes, DataType.Address).bytes))

        newState.accountTransactionIds(master, 5, 0)._2.size shouldBe 5 // genesis, reg, deposit, lock, withdraw
        // lock contract
        newState.contractContent(contractId).get._1 shouldBe 2
        newState.contractContent(contractId).get._2.arr shouldEqual reg.id.arr
        newState.contractContent(contractId).get._3.bytes.arr shouldEqual ContractLock.contract.bytes.arr
        // lock contract token Info
        newState.contractInfo(makerKey).get.bytes shouldEqual DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes
        newState.contractInfo(contractTokenIdKey).get.bytes shouldEqual DataEntry(VSYSId.arr, DataType.TokenId).bytes
        newState.contractNumInfo(masterBalanceInContractKey) shouldBe 10000L - 1L
        newState.contractInfo(masterLockTimeInContractKey).get.bytes shouldBe DataEntry(Longs.toByteArray(lockV.timestamp), DataType.Timestamp).bytes
        // VSYS balance
        newState.balance(master.toAddress) shouldBe ENOUGH_AMT - 10000L + 1L - 4 * fee - 10000000000L
        newState.balance(reg.contractId) shouldBe 9999L
      }
    }
  }

}
