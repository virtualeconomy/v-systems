package vsys.blockchain.state.contract.swap

import cats.Monoid
import com.google.common.primitives.{Bytes, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.hash.Sha256
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.contract._
import vsys.blockchain.contract.swap.AtomicSwapContractGen
import vsys.blockchain.state.{ByteStr, Portfolio}
import vsys.blockchain.state.diffs.assertDiffAndStateCorrectBlockTime
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}

class AtomicSwapContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with AtomicSwapContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsSwapContractAndSolvePuzzle: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach) <- createAndDepositVSYSAtomicSwapContractGen(1000L)
    lockData = Seq(Longs.toByteArray(50L), user.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regContract.contractId, lockData, lockType, attach, fee, ts)
    solvePuzzleData = Seq(lock.id.arr, Longs.toByteArray(123L))
    solvePuzzleType = Seq(DataType.ShortBytes, DataType.ShortBytes)
    solvePuzzle <- solvePuzzleAtomicSwapContractDataStackGen(user, regContract.contractId, solvePuzzleData, solvePuzzleType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, lock, solvePuzzle, ts, fee)

  property("solve puzzle correctly allocates VSYS") {
    forAll(preconditionsSwapContractAndSolvePuzzle) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, solve: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.create(lock.timestamp + 1, Seq(lock, solve))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -2 * fee
        totalPortfolioDiff.effectiveBalance shouldBe -2 * fee

//        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
//        val user = solve.proofs.firstCurveProof.explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val contractId = reg.contractId.bytes

        // StateVar Keys
//        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
//        val contractTokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        // StateMap Keys
        val masterBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val userBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(userBytes, DataType.Address).bytes))

        newState.contractNumInfo(masterBalanceInContractKey) shouldBe 1000L - 50L // deposited - locked
        newState.contractNumInfo(userBalanceInContractKey) shouldBe 50L // solving puzzle allocates the locked funds

      }
    }
  }

  val preconditionsSwapContractAndSolvePuzzleTokens: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenContract, issueToken, regContract, depositToken, ts, fee, description, attach) <- createAndDepositTokenAtomicSwapContractGen(1000000L, 100L, 1000L, 1000L)
    lockData = Seq(Longs.toByteArray(50L), user.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regContract.contractId, lockData, lockType, attach, fee, ts)
    solvePuzzleData = Seq(lock.id.arr, Longs.toByteArray(123L))
    solvePuzzleType = Seq(DataType.ShortBytes, DataType.ShortBytes)
    solvePuzzle <- solvePuzzleAtomicSwapContractDataStackGen(user, regContract.contractId, solvePuzzleData, solvePuzzleType, attach, fee, ts)
  } yield (genesis, genesis2, regTokenContract, issueToken, regContract, depositToken, lock, solvePuzzle, ts, fee)

  property("solve puzzle correctly allocates tokens") {
    forAll(preconditionsSwapContractAndSolvePuzzleTokens) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction,
    regToken: RegisterContractTransaction, issue: ExecuteContractFunctionTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, solve: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(regToken, issue, reg, deposit))),
        TestBlock.create(lock.timestamp + 1, Seq(lock, solve))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -2 * fee
        totalPortfolioDiff.effectiveBalance shouldBe -2 * fee

//        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
//        val user = solve.proofs.firstCurveProof.explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val contractId = reg.contractId.bytes

        // StateVar Keys
//        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
//        val contractTokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        // StateMap Keys
        val masterBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val userBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(userBytes, DataType.Address).bytes))

        newState.contractNumInfo(masterBalanceInContractKey) shouldBe 1000L - 50L // deposited - locked
        newState.contractNumInfo(userBalanceInContractKey) shouldBe 50L // solving puzzle allocates the locked funds

      }
    }
  }

  val preconditionsSwapContractAndSwapTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenContract, regToken2Contract, regSwapContract, regSwap2Contract, issueToken, issueToken2,
    depositToken, deposit2Token, ts, fee, description, attach) <- createTwoTokenAndDepositSwapGen(100000000L, 100L, 100000L, 10000L)
    // Lock first swap
    lockData = Seq(Longs.toByteArray(50L), user.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regSwapContract.contractId, lockData, lockType, attach, fee, ts)
    solvePuzzleData = Seq(lock.id.arr, Longs.toByteArray(123L))
    solvePuzzleType = Seq(DataType.ShortBytes, DataType.ShortBytes)
    solvePuzzle <- solvePuzzleAtomicSwapContractDataStackGen(user, regSwapContract.contractId, solvePuzzleData, solvePuzzleType, attach, fee, ts)
    // Lock second swap
    lock2Data = Seq(Longs.toByteArray(50L), master.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 50))
    lock2Type = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock2 <- lockAtomicSwapContractDataStackGen(user, regSwap2Contract.contractId, lock2Data, lock2Type, attach, fee, ts)
    solvePuzzle2Data = Seq(lock2.id.arr, Longs.toByteArray(123L))
    solvePuzzle2Type = Seq(DataType.ShortBytes, DataType.ShortBytes)
    solvePuzzle2 <- solvePuzzleAtomicSwapContractDataStackGen(master, regSwap2Contract.contractId, solvePuzzle2Data, solvePuzzle2Type, attach, fee, ts)
  } yield (genesis, genesis2, regTokenContract, regToken2Contract, regSwapContract, regSwap2Contract, issueToken, issueToken2,
    depositToken, deposit2Token, lock, lock2, solvePuzzle, solvePuzzle2, ts, fee)

  property("swap handled correctly") {
    forAll(preconditionsSwapContractAndSwapTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regToken: RegisterContractTransaction,
    regToken2: RegisterContractTransaction, regSwap: RegisterContractTransaction, regSwap2: RegisterContractTransaction, issue: ExecuteContractFunctionTransaction,
    issue2: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction, deposit2: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction,
    lock2: ExecuteContractFunctionTransaction, solve: ExecuteContractFunctionTransaction, solve2: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(regToken,
        regToken2, regSwap, regSwap2, issue, issue2, deposit, deposit2))),
        TestBlock.create(lock.timestamp + 1, Seq(lock, lock2, solve, solve2))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -4 * fee
        totalPortfolioDiff.effectiveBalance shouldBe -4 * fee

//        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
//        val user = solve.proofs.firstCurveProof.explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val contractId = regSwap.contractId.bytes
        val contract2Id = regSwap2.contractId.bytes

        // Contract 1

        // StateMap Keys
        val masterBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val userBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(userBytes, DataType.Address).bytes))

        newState.contractNumInfo(masterBalanceInContractKey) shouldBe 10000L - 50L // deposited - locked
        newState.contractNumInfo(userBalanceInContractKey) shouldBe 50L // solving puzzle allocates the locked funds

        // Contract 2
         val masterBalanceInContract2Key = ByteStr(Bytes.concat(contract2Id.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
         val userBalanceInContract2Key = ByteStr(Bytes.concat(contract2Id.arr, Array(0.toByte), DataEntry(userBytes, DataType.Address).bytes))

         newState.contractNumInfo(userBalanceInContract2Key) shouldBe 10000L - 50L // deposited - locked
         newState.contractNumInfo(masterBalanceInContract2Key) shouldBe 50L // solving puzzle allocates the locked funds

      }
    }
  }

  val preconditionsAndWithdrawExpiredSwapVSYS: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, ts, fee, description, attach) <- createAndDepositVSYSAtomicSwapContractGen(1000L)
    lockData = Seq(Longs.toByteArray(50L), user.toAddress.bytes.arr, Sha256(Longs.toByteArray(123L)), Longs.toByteArray(ts + 100))
    lockType = Seq(DataType.Amount, DataType.Address, DataType.ShortBytes, DataType.Timestamp)
    lock <- lockAtomicSwapContractDataStackGen(master, regContract.contractId, lockData, lockType, attach, fee, ts)
    withdrawData = Seq(lock.id.arr)
    withdrawType = Seq(DataType.ShortBytes)
    withdraw <- expireWithdrawAtomicSwapContractDataStackGen(master, regContract.contractId, withdrawData, withdrawType, attach, fee, ts)
    withdrawVSYSData = Seq(regContract.contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(1000L))
    withdrawVSYSType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdrawVSYS <- withdrawVSYSGen(master, withdrawVSYSData, withdrawVSYSType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, lock, withdraw, withdrawVSYS, ts, fee)

  property("expired swap allows withdrawal") {
    forAll(preconditionsAndWithdrawExpiredSwapVSYS) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, lock: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction,
    withdrawVSYS: ExecuteContractFunctionTransaction, ts: Long, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.create(lock.timestamp + 110, Seq(lock, withdraw, withdrawVSYS))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -3 * fee
        totalPortfolioDiff.effectiveBalance shouldBe -3 * fee

        val masterBytes = genesis.recipient.bytes.arr
        val contractId = reg.contractId.bytes

        // StateMap Keys
        val masterBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))

        newState.contractNumInfo(masterBalanceInContractKey) shouldBe 0L
      }
    }
  }
}
