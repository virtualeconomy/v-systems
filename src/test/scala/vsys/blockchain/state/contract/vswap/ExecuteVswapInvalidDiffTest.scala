package vsys.blockchain.state.contract.vswap

import com.google.common.primitives.Longs
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.contract.vswap.VswapContractGen
import vsys.blockchain.state.ByteStr
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.state._
import com.google.common.primitives.{Bytes, Ints, Longs}

class ExecuteVswapInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with VswapContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVSwapDepositAndWithdrawLiquidityTokens: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <- createABLiquidityTokenAndInitVSwap(1000, 1, 1000, 1000, 1, 1000,
      1000, 1000, 1000,10, 100, 100)
    //withdraw tokens
    withdrawLiquidityData = Seq(regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(100))
    withdrawLiquidityType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdrawLiquidity <- withdrawTokenGen(master, regLiquidity.contractId, false, withdrawLiquidityData, withdrawLiquidityType, attach, fee, ts + 12)

    withdrawInvalidLiquidityData = Seq(regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(2000))
    withdrawInvalidLiquidity <- withdrawTokenGen(master, regLiquidity.contractId, false, withdrawInvalidLiquidityData, withdrawLiquidityType, attach, fee, ts + 12)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity,
    withdrawLiquidity, withdrawInvalidLiquidity, fee, ts)

  property("withdraw liquidity tokens more than depositing in vswap contract") {
    forAll(preconditionsAndVSwapDepositAndWithdrawLiquidityTokens) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction,
    withdrawLiquidity: ExecuteContractFunctionTransaction, withdrawInvalidLiquidity: ExecuteContractFunctionTransaction,  _, _) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(withdrawLiquidity.timestamp, Seq(withdrawLiquidity), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(withdrawInvalidLiquidity.timestamp, Seq(withdrawInvalidLiquidity), TransactionStatus.ContractTokenBalanceInsufficient)) { (blockDiffEi) =>

        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient

      }
    }
  }

  val preconditionsAndVSwapSetSwap: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(1000, 1, 1000,
        1000, 1, 1000,
        1000, 1000, 1000,9, 100,
        100)
    // set swap
    setSwapAmountADesired = 10L
    setSwapAmountBDesired = 10L
    setSwapData = Seq(Longs.toByteArray(setSwapAmountADesired), Longs.toByteArray(setSwapAmountBDesired))
    setSwapDataType = Seq(DataType.Amount, DataType.Amount)
    setSwap <- setSwapGen(master, regVSwap.contractId, setSwapData, setSwapDataType, attach, fee + 10000000000L, ts)

    setInvalidSwapData = Seq(Longs.toByteArray(1000), Longs.toByteArray(1000))
    setInvalidSwap <- setSwapGen(master, regVSwap.contractId, setInvalidSwapData, setSwapDataType, attach, fee + 10000000000L, ts)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, setInvalidSwap, fee, ts)

  property("set swap more than deposit") {
    forAll(preconditionsAndVSwapSetSwap) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, setInvalidSwap: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(setSwap), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(setInvalidSwap.timestamp, Seq(setInvalidSwap), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  val preconditionsAndVSwapAddLiquidity: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000,
        10000, 1, 10000,
        10000, 1000, 10000,10, 3000,
        3000)
    // set swap
    setSwapAmountADesired = 1000L
    setSwapAmountBDesired = 1000L
    setSwapData = Seq(Longs.toByteArray(setSwapAmountADesired), Longs.toByteArray(setSwapAmountBDesired))
    setSwapDataType = Seq(DataType.Amount, DataType.Amount)
    setSwap <- setSwapGen(master, regVSwap.contractId, setSwapData, setSwapDataType, attach, fee + 10000000000L, ts)

    // add liquidity
    addLiquidityAmountADesired = 2000L
    addLiquidityAmountBDesired = 2000L
    addLiquidityMinA = 900L
    addLiquidityMinB = 900L
    addLiquidityDeadline = ts + 1000000000000L
    addLiquidityData = Seq(Longs.toByteArray(addLiquidityAmountADesired), Longs.toByteArray(addLiquidityAmountBDesired),
      Longs.toByteArray(addLiquidityMinA), Longs.toByteArray(addLiquidityMinB), Longs.toByteArray(addLiquidityDeadline))
    addLiquidityDataType = Seq(DataType.Amount, DataType.Amount, DataType.Amount, DataType.Amount, DataType.Timestamp)
    addLiquidity <- addLiquidityGen(master, regVSwap.contractId, addLiquidityData, addLiquidityDataType, attach, fee + 10000000000L, ts)

    addLiquidityAmountADesired = 3000L
    addLiquidityAmountBDesired = 3000L
    addLiquidityMinA = 900L
    addLiquidityMinB = 900L
    addInvalidLiquidityData = Seq(Longs.toByteArray(addLiquidityAmountADesired), Longs.toByteArray(addLiquidityAmountBDesired),
      Longs.toByteArray(addLiquidityMinA), Longs.toByteArray(addLiquidityMinB), Longs.toByteArray(addLiquidityDeadline))
    addInvalidLiquidity <- addLiquidityGen(master, regVSwap.contractId, addInvalidLiquidityData, addLiquidityDataType, attach, fee + 10000000000L, ts)

    addLiquidityAmountADesired = 2000L
    addLiquidityAmountBDesired = 2000L
    addLiquidityMinA = 9000L
    addLiquidityMinB = 9000L
    addInvalidLiquidityData2 = Seq(Longs.toByteArray(addLiquidityAmountADesired), Longs.toByteArray(addLiquidityAmountBDesired),
      Longs.toByteArray(addLiquidityMinA), Longs.toByteArray(addLiquidityMinB), Longs.toByteArray(addLiquidityDeadline))
    addInvalidLiquidity2 <- addLiquidityGen(master, regVSwap.contractId, addInvalidLiquidityData2, addLiquidityDataType, attach, fee + 10000000000L, ts)

    addLiquidityAmountADesired = 2000L
    addLiquidityAmountBDesired = 2000L
    addLiquidityMinA = 900L
    addLiquidityMinB = 900L
    addInvalidLiquidityDeadline = ts - 1000000000000L
    addInvalidLiquidityData3 = Seq(Longs.toByteArray(addLiquidityAmountADesired), Longs.toByteArray(addLiquidityAmountBDesired),
      Longs.toByteArray(addLiquidityMinA), Longs.toByteArray(addLiquidityMinB), Longs.toByteArray(addInvalidLiquidityDeadline))
    addInvalidLiquidity3 <- addLiquidityGen(master, regVSwap.contractId, addInvalidLiquidityData3, addLiquidityDataType, attach, fee + 10000000000L, ts)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, addLiquidity, addInvalidLiquidity, addInvalidLiquidity2, addInvalidLiquidity3, fee, ts)

  property("add liquidity less than addLiquidityMin") {
    forAll(preconditionsAndVSwapAddLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, addLiquidity: ExecuteContractFunctionTransaction, addInvalidLiquidity: ExecuteContractFunctionTransaction, addInvalidLiquidity2: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addLiquidity.timestamp, Seq(addLiquidity), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addInvalidLiquidity2.timestamp, Seq(addInvalidLiquidity2), TransactionStatus.Failed)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed

      }
    }
  }
  property("vswap add liquidity more than deposit") {
    forAll(preconditionsAndVSwapAddLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, addLiquidity: ExecuteContractFunctionTransaction,
    addInvalidLiquidity: ExecuteContractFunctionTransaction, _, _,  _, _) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addInvalidLiquidity.timestamp, Seq(addInvalidLiquidity), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  property("add liquidity with wrong addInvalidLiquidityDeadline") {
    forAll(preconditionsAndVSwapAddLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, addLiquidity: ExecuteContractFunctionTransaction,
    addInvalidLiquidity: ExecuteContractFunctionTransaction, addInvalidLiquidity2: ExecuteContractFunctionTransaction, addInvalidLiquidity3: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addInvalidLiquidity3.timestamp, Seq(addInvalidLiquidity3), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVSwapRemoveLiquidity: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000,
        10000, 1, 10000,
        10000, 1000, 10000,10, 2000,
        2000)
    // set swap
    setSwapAmountADesired = 1000L
    setSwapAmountBDesired = 1000L
    setSwapData = Seq(Longs.toByteArray(setSwapAmountADesired), Longs.toByteArray(setSwapAmountBDesired))
    setSwapDataType = Seq(DataType.Amount, DataType.Amount)
    setSwap <- setSwapGen(master, regVSwap.contractId, setSwapData, setSwapDataType, attach, fee + 10000000000L, ts)

    // remove liquidity
    removeLiquidityDesired = 100L
    removeLiquidityAmountADesired = 100L
    removeLiquidityAmountBDesired = 100L
    removeLiquidityDeadline = ts + 1000000000000L
    removeLiquidityData = Seq(Longs.toByteArray(removeLiquidityDesired), Longs.toByteArray(removeLiquidityAmountADesired),
      Longs.toByteArray(removeLiquidityAmountBDesired), Longs.toByteArray(removeLiquidityDeadline))
    removeLiquidityDataType = Seq(DataType.Amount, DataType.Amount, DataType.Amount, DataType.Timestamp)
    removeLiquidity <- removeLiquidityGen(master, regVSwap.contractId, removeLiquidityData, removeLiquidityDataType, attach, fee + 10000000000L, ts)

    removeLiquidityDesired = 2000L
    removeLiquidityAmountADesired = 2000L
    removeLiquidityAmountBDesired = 2000L
    removeInvalidLiquidityData = Seq(Longs.toByteArray(removeLiquidityDesired), Longs.toByteArray(removeLiquidityAmountADesired),
      Longs.toByteArray(removeLiquidityAmountBDesired), Longs.toByteArray(removeLiquidityDeadline))
    removeInvalidLiquidity <- removeLiquidityGen(master, regVSwap.contractId, removeInvalidLiquidityData, removeLiquidityDataType, attach, fee + 10000000000L, ts)

    removeLiquidityDesired = 100L
    removeLiquidityAmountADesired = 200L
    removeLiquidityAmountBDesired = 200L
    removeInvalidLiquidityData2 = Seq(Longs.toByteArray(removeLiquidityDesired), Longs.toByteArray(removeLiquidityAmountADesired),
      Longs.toByteArray(removeLiquidityAmountBDesired), Longs.toByteArray(removeLiquidityDeadline))
    removeInvalidLiquidity2 <- removeLiquidityGen(master, regVSwap.contractId, removeInvalidLiquidityData2, removeLiquidityDataType, attach, fee + 10000000000L, ts)

    removeLiquidityDesired = 100L
    removeLiquidityAmountADesired = 100L
    removeLiquidityAmountBDesired = 100L
    removeLiquidityDeadline = ts - 1000000000000L
    removeInvalidLiquidityData3 = Seq(Longs.toByteArray(removeLiquidityDesired), Longs.toByteArray(removeLiquidityAmountADesired),
      Longs.toByteArray(removeLiquidityAmountBDesired), Longs.toByteArray(removeLiquidityDeadline))
    removeInvalidLiquidity3 <- removeLiquidityGen(master, regVSwap.contractId, removeInvalidLiquidityData3, removeLiquidityDataType, attach, fee + 10000000000L, ts)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, removeLiquidity, removeInvalidLiquidity, removeInvalidLiquidity2, removeInvalidLiquidity3, fee, ts)

  property("remove liquidity more than in pool") {
    forAll(preconditionsAndVSwapRemoveLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction,
    removeLiquidity: ExecuteContractFunctionTransaction, removeInvalidLiquidity: ExecuteContractFunctionTransaction, removeInvalidLiquidity2: ExecuteContractFunctionTransaction, _, _, _) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeLiquidity.timestamp, Seq(removeLiquidity), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeInvalidLiquidity.timestamp, Seq(removeInvalidLiquidity), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  property("remove liquidity more than max removeLiquidityDesired") {
    forAll(preconditionsAndVSwapRemoveLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction,
    removeLiquidity: ExecuteContractFunctionTransaction, removeInvalidLiquidity: ExecuteContractFunctionTransaction, removeInvalidLiquidity2: ExecuteContractFunctionTransaction, _, _, _) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeInvalidLiquidity2.timestamp, Seq(removeInvalidLiquidity2), TransactionStatus.Failed)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
  property("remove liquidity with wrong removeLiquidityDeadline") {
    forAll(preconditionsAndVSwapRemoveLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction,
    removeLiquidity: ExecuteContractFunctionTransaction, removeInvalidLiquidity: ExecuteContractFunctionTransaction, removeInvalidLiquidity2: ExecuteContractFunctionTransaction, removeInvalidLiquidity3: ExecuteContractFunctionTransaction, _, _) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeInvalidLiquidity3.timestamp, Seq(removeInvalidLiquidity3), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVSwapSwapTokenForExactBaseToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000,
        10000, 1, 10000,
        10000, 1000, 10000,10, 2000,
        2000)
    // set swap
    setSwapAmountADesired = 1000L
    setSwapAmountBDesired = 1000L
    setSwapData = Seq(Longs.toByteArray(setSwapAmountADesired), Longs.toByteArray(setSwapAmountBDesired))
    setSwapDataType = Seq(DataType.Amount, DataType.Amount)
    setSwap <- setSwapGen(master, regVSwap.contractId, setSwapData, setSwapDataType, attach, fee + 10000000000L, ts)

    // swap for 100 base tokens
    swapAmountOut = 100L
    swapAmountInMax = 112L
    swapDeadline = ts + 1000000000000L
    swapData = Seq(Longs.toByteArray(swapAmountOut), Longs.toByteArray(swapAmountInMax), Longs.toByteArray(swapDeadline))
    swapDataType = Seq(DataType.Amount, DataType.Amount, DataType.Timestamp)
    swapTokenForExactBaseToken <- swapTokenForExactBaseTokenGen(master, regVSwap.contractId, swapData, swapDataType, attach, fee + 10000000000L, ts)

    swapAmountOut = 100L
    swapAmountInMax = 1L
    swapDeadline = ts + 1000000000000L
    swapInvalidData = Seq(Longs.toByteArray(swapAmountOut), Longs.toByteArray(swapAmountInMax), Longs.toByteArray(swapDeadline))
    swapInvalidTokenForExactBaseToken <- swapTokenForExactBaseTokenGen(master, regVSwap.contractId, swapInvalidData, swapDataType, attach, fee + 10000000000L, ts)

    swapAmountOut = 100L
    swapAmountInMax = 112L
    swapDeadline = ts - 1000000000000L
    swapInvalidData2 = Seq(Longs.toByteArray(swapAmountOut), Longs.toByteArray(swapAmountInMax), Longs.toByteArray(swapDeadline))
    swapInvalidTokenForExactBaseToken2 <- swapTokenForExactBaseTokenGen(master, regVSwap.contractId, swapInvalidData2, swapDataType, attach, fee + 10000000000L, ts)

} yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapTokenForExactBaseToken, swapInvalidTokenForExactBaseToken, swapInvalidTokenForExactBaseToken2, fee, ts)

  property("swap tokens for exact base token with wrong total supply") {
    forAll(preconditionsAndVSwapSwapTokenForExactBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactBaseToken: ExecuteContractFunctionTransaction, swapInvalidTokenForExactBaseToken: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapTokenForExactBaseToken), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success

      }
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidTokenForExactBaseToken.timestamp, Seq(swapInvalidTokenForExactBaseToken), TransactionStatus.Failed)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed

      }
    }
  }
  property("swap tokens for exact base token with wrong swapDeadline") {
    forAll(preconditionsAndVSwapSwapTokenForExactBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactBaseToken: ExecuteContractFunctionTransaction,
    swapInvalidTokenForExactBaseToken: ExecuteContractFunctionTransaction, swapInvalidTokenForExactBaseToken2: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidTokenForExactBaseToken2.timestamp, Seq(swapInvalidTokenForExactBaseToken2), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed

      }
    }
  }

  val preconditionsAndVSwapSwapExactTokenForBaseToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000,
        10000, 1, 10000,
        10000, 1000, 10000,10, 2000,
        2000)
    // set swap
    setSwapAmountADesired = 1000L
    setSwapAmountBDesired = 1000L
    setSwapData = Seq(Longs.toByteArray(setSwapAmountADesired), Longs.toByteArray(setSwapAmountBDesired))
    setSwapDataType = Seq(DataType.Amount, DataType.Amount)
    setSwap <- setSwapGen(master, regVSwap.contractId, setSwapData, setSwapDataType, attach, fee + 10000000000L, ts)

    // swap exact token for base token
    swapAmountOutMin = 496
    swapAmountIn = 990L
    swapDeadline = ts + 1000000000000L
    swapData = Seq(Longs.toByteArray(swapAmountOutMin), Longs.toByteArray(swapAmountIn), Longs.toByteArray(swapDeadline))
    swapDataType = Seq(DataType.Amount, DataType.Amount, DataType.Timestamp)
    swapExactTokenForBaseToken <- swapExactTokenForBaseTokenGen(master, regVSwap.contractId, swapData, swapDataType, attach, fee + 10000000000L, ts)

    swapAmountOutMin = 497
    swapAmountIn = 990L
    swapInvalidData = Seq(Longs.toByteArray(swapAmountOutMin), Longs.toByteArray(swapAmountIn), Longs.toByteArray(swapDeadline))
    swapInvalidExactTokenForBaseToken <- swapExactTokenForBaseTokenGen(master, regVSwap.contractId, swapInvalidData, swapDataType, attach, fee + 10000000000L, ts)

    swapAmountOutMin = 496
    swapAmountIn = 990L
    swapDeadline = ts - 1000000000000L
    swapInvalidData2 = Seq(Longs.toByteArray(swapAmountOutMin), Longs.toByteArray(swapAmountIn), Longs.toByteArray(swapDeadline))
    swapInvalidExactTokenForBaseToken2 <- swapExactTokenForBaseTokenGen(master, regVSwap.contractId, swapInvalidData2, swapDataType, attach, fee + 10000000000L, ts)

} yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapExactTokenForBaseToken, swapInvalidExactTokenForBaseToken, swapInvalidExactTokenForBaseToken2, fee, ts)

  property("swap exact token for base token with wrong total supply") {
    forAll(preconditionsAndVSwapSwapExactTokenForBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForBaseToken: ExecuteContractFunctionTransaction,
    swapInvalidExactTokenForBaseToken: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapExactTokenForBaseToken), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidExactTokenForBaseToken.timestamp, Seq(swapInvalidExactTokenForBaseToken), TransactionStatus.Failed)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
  property("swap exact token for base token with wrong swapDeadline") {
    forAll(preconditionsAndVSwapSwapExactTokenForBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForBaseToken: ExecuteContractFunctionTransaction,
    swapInvalidExactTokenForBaseToken: ExecuteContractFunctionTransaction, swapInvalidExactTokenForBaseToken2: ExecuteContractFunctionTransaction, _, _) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidExactTokenForBaseToken2.timestamp, Seq(swapInvalidExactTokenForBaseToken2), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVSwapSwapTokenForExactTargetToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000,
        10000, 1, 10000,
        10000, 1000, 10000,10, 2000,
        2000)
    // set swap
    setSwapAmountADesired = 1000L
    setSwapAmountBDesired = 1000L
    setSwapData = Seq(Longs.toByteArray(setSwapAmountADesired), Longs.toByteArray(setSwapAmountBDesired))
    setSwapDataType = Seq(DataType.Amount, DataType.Amount)
    setSwap <- setSwapGen(master, regVSwap.contractId, setSwapData, setSwapDataType, attach, fee + 10000000000L, ts)

    // swap exact token for base token
    swapAmountOut = 100L
    swapAmountInMax = 112L
    swapDeadline = ts + 1000000000000L
    swapData = Seq(Longs.toByteArray(swapAmountOut), Longs.toByteArray(swapAmountInMax), Longs.toByteArray(swapDeadline))
    swapDataType = Seq(DataType.Amount, DataType.Amount, DataType.Timestamp)
    swapTokenForExactTargetToken <- swapTokenForExactTargetTokenGen(master, regVSwap.contractId, swapData, swapDataType, attach, fee + 10000000000L, ts)

    swapAmountOut = 100L
    swapAmountInMax = 111L
    swapInvalidData = Seq(Longs.toByteArray(swapAmountOut), Longs.toByteArray(swapAmountInMax), Longs.toByteArray(swapDeadline))
    swapInvalidTokenForExactTargetToken <- swapTokenForExactTargetTokenGen(master, regVSwap.contractId, swapInvalidData, swapDataType, attach, fee + 10000000000L, ts)

    swapAmountOut = 100L
    swapAmountInMax = 112L
    swapDeadline = ts - 1000000000000L
    swapInvalidData2 = Seq(Longs.toByteArray(swapAmountOut), Longs.toByteArray(swapAmountInMax), Longs.toByteArray(swapDeadline))
    swapInvalidTokenForExactTargetToken2 <- swapTokenForExactTargetTokenGen(master, regVSwap.contractId, swapInvalidData2, swapDataType, attach, fee + 10000000000L, ts)

} yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapTokenForExactTargetToken, swapInvalidTokenForExactTargetToken, swapInvalidTokenForExactTargetToken2, fee, ts)

  property("swap token for exact target token with wrong total supply") {
    forAll(preconditionsAndVSwapSwapTokenForExactTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactTargetToken: ExecuteContractFunctionTransaction,
    swapInvalidTokenForExactTargetToken: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapTokenForExactTargetToken), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidTokenForExactTargetToken.timestamp, Seq(swapInvalidTokenForExactTargetToken), TransactionStatus.Failed)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("swap token for exact target token with wrong swapDeadline") {
    forAll(preconditionsAndVSwapSwapTokenForExactTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactTargetToken: ExecuteContractFunctionTransaction,
    swapInvalidTokenForExactTargetToken: ExecuteContractFunctionTransaction, swapInvalidTokenForExactTargetToken2: ExecuteContractFunctionTransaction, _, _) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidTokenForExactTargetToken2.timestamp, Seq(swapInvalidTokenForExactTargetToken2), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
  val preconditionsAndVSwapSwapExactTokenForTargetToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000,
        10000, 1, 10000,
        10000, 1000, 10000,10, 2000,
        2000)
    // set swap
    setSwapAmountADesired = 1000L
    setSwapAmountBDesired = 1000L
    setSwapData = Seq(Longs.toByteArray(setSwapAmountADesired), Longs.toByteArray(setSwapAmountBDesired))
    setSwapDataType = Seq(DataType.Amount, DataType.Amount)
    setSwap <- setSwapGen(master, regVSwap.contractId, setSwapData, setSwapDataType, attach, fee + 10000000000L, ts)

    // swap exact token for base token
    swapAmountOutMin = 90L
    swapAmountIn = 100L
    swapDeadline = ts + 1000000000000L
    swapData = Seq(Longs.toByteArray(swapAmountOutMin), Longs.toByteArray(swapAmountIn), Longs.toByteArray(swapDeadline))
    swapDataType = Seq(DataType.Amount, DataType.Amount, DataType.Timestamp)
    swapExactTokenForTargetToken <- swapExactTokenForTargetTokenGen(master, regVSwap.contractId, swapData, swapDataType, attach, fee + 10000000000L, ts)

    swapAmountOutMin = 91L
    swapAmountIn = 100L
    swapInvalidData = Seq(Longs.toByteArray(swapAmountOutMin), Longs.toByteArray(swapAmountIn), Longs.toByteArray(swapDeadline))
    swapInvalidExactTokenForTargetToken <- swapExactTokenForTargetTokenGen(master, regVSwap.contractId, swapInvalidData, swapDataType, attach, fee + 10000000000L, ts)

    swapAmountOutMin = 90L
    swapAmountIn = 100L
    swapDeadline = ts - 1000000000000L
    swapInvalidData2 = Seq(Longs.toByteArray(swapAmountOutMin), Longs.toByteArray(swapAmountIn), Longs.toByteArray(swapDeadline))
    swapInvalidExactTokenForTargetToken2 <- swapExactTokenForTargetTokenGen(master, regVSwap.contractId, swapInvalidData2, swapDataType, attach, fee + 10000000000L, ts)

} yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapExactTokenForTargetToken, swapInvalidExactTokenForTargetToken, swapInvalidExactTokenForTargetToken2, fee, ts)

  property("swap exact token for target token with wrong total supply") {
    forAll(preconditionsAndVSwapSwapExactTokenForTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForTargetToken: ExecuteContractFunctionTransaction,
    swapInvalidExactTokenForTargetToken: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapExactTokenForTargetToken), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidExactTokenForTargetToken.timestamp, Seq(swapInvalidExactTokenForTargetToken), TransactionStatus.Failed)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
  property("swap exact token for target token with wrong swapDeadline") {
    forAll(preconditionsAndVSwapSwapExactTokenForTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForTargetToken: ExecuteContractFunctionTransaction,
    swapInvalidExactTokenForTargetToken: ExecuteContractFunctionTransaction, swapInvalidExactTokenForTargetToken2: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidExactTokenForTargetToken2.timestamp, Seq(swapInvalidExactTokenForTargetToken2), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

}
