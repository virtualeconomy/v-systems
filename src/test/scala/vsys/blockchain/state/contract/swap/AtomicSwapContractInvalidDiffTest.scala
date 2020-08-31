package vsys.blockchain.state.contract.swap

//import cats.Monoid
import com.google.common.primitives.Longs
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.hash.Sha256
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.contract._
import vsys.blockchain.contract.swap.AtomicSwapContractGen
import vsys.blockchain.state.diffs.assertDiffAndStateCorrectBlockTime
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

class AtomicSwapContractInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with AtomicSwapContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndAtomicSwapInsufficientDepositedAmountInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach) <- createAndDepositVSYSAtomicSwapContractGen(1000L)
    lockData = Seq(Longs.toByteArray(1001L), user.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regContract.contractId, lockData, lockType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, lock, ts, fee)

  property("unable to lock more than deposited") {
    forAll(preconditionsAndAtomicSwapInsufficientDepositedAmountInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.createWithTxStatus(lock.timestamp + 1, Seq(lock), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  val preconditionsAndAtomicSwapExpiredWithdrawInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach) <- createAndDepositVSYSAtomicSwapContractGen(1000L)
    lockData = Seq(Longs.toByteArray(1000L), user.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regContract.contractId, lockData, lockType, attach, fee, ts)
    withdrawData = Seq(lock.id.arr)
    withdrawType = Seq(DataType.ShortBytes)
    withdraw <- expireWithdrawAtomicSwapContractDataStackGen(master, regContract.contractId, withdrawData, withdrawType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, lock, withdraw, ts, fee)

  property("unable to withdraw before lock expires") {
    forAll(preconditionsAndAtomicSwapExpiredWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, lock))),
        TestBlock.createWithTxStatus(lock.timestamp + 99, Seq(withdraw), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndAtomicSwapLockWrongAddressInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach) <- createAndDepositVSYSAtomicSwapContractGen(1000L)
    lockData = Seq(Longs.toByteArray(1000L), user.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(user, regContract.contractId, lockData, lockType, attach, fee, ts + 3)
  } yield (genesis, genesis2, regContract, depositVSYS, lock, ts, fee)

  property("wrong address cannot lock contract") {
    forAll(preconditionsAndAtomicSwapLockWrongAddressInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.createWithTxStatus(lock.timestamp + 1, Seq(lock), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  val preconditionsAndAtomicSwapSolvePuzzleWrongAddressInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach) <- createAndDepositVSYSAtomicSwapContractGen(1000L)
    lockData = Seq(Longs.toByteArray(1000L), master.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regContract.contractId, lockData, lockType, attach, fee, ts)
    solvePuzzleData = Seq(lock.id.arr, Longs.toByteArray(123L))
    solvePuzzleType = Seq(DataType.ShortBytes, DataType.ShortBytes)
    solvePuzzle <- solvePuzzleAtomicSwapContractDataStackGen(user, regContract.contractId, solvePuzzleData, solvePuzzleType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, lock, solvePuzzle, ts, fee)

  property("wrong address cannot solve puzzle") {
    forAll(preconditionsAndAtomicSwapSolvePuzzleWrongAddressInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, solvePuzzle: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, lock))),
        TestBlock.createWithTxStatus(lock.timestamp + 1, Seq(solvePuzzle), TransactionStatus.ContractInvalidCaller)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }
    }
  }

  val preconditionsAndAtomicSwapSolvePuzzleExpiredInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach) <- createAndDepositVSYSAtomicSwapContractGen(1000L)
    lockData = Seq(Longs.toByteArray(1000L), master.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regContract.contractId, lockData, lockType, attach, fee, ts)
    solvePuzzleData = Seq(lock.id.arr, Longs.toByteArray(123L))
    solvePuzzleType = Seq(DataType.ShortBytes, DataType.ShortBytes)
    solvePuzzle <- solvePuzzleAtomicSwapContractDataStackGen(user, regContract.contractId, solvePuzzleData, solvePuzzleType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, lock, solvePuzzle, ts, fee)

  property("puzzle cannot be solved after the lock expires") {
    forAll(preconditionsAndAtomicSwapSolvePuzzleExpiredInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, solvePuzzle: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, lock))),
        TestBlock.createWithTxStatus(lock.timestamp + 101, Seq(solvePuzzle), TransactionStatus.ContractInvalidCaller)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }
    }
  }

  val preconditionsAndAtomicSwapSolvePuzzleInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach) <- createAndDepositVSYSAtomicSwapContractGen(1000L)
    lockData = Seq(Longs.toByteArray(1000L), user.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regContract.contractId, lockData, lockType, attach, fee, ts)
    solvePuzzleData = Seq(lock.id.arr, Longs.toByteArray(124L))
    solvePuzzleType = Seq(DataType.ShortBytes, DataType.ShortBytes)
    solvePuzzle <- solvePuzzleAtomicSwapContractDataStackGen(user, regContract.contractId, solvePuzzleData, solvePuzzleType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, lock, solvePuzzle, ts, fee)

  property("puzzle cannot be solved by incorrect key") {
    forAll(preconditionsAndAtomicSwapSolvePuzzleInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, solvePuzzle: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, lock))),
        TestBlock.createWithTxStatus(lock.timestamp + 1, Seq(solvePuzzle), TransactionStatus.ContractInvalidHash)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidHash
      }
    }
  }

}