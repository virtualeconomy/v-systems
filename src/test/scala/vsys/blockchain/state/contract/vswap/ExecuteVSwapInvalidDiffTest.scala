package vsys.blockchain.state.contract.vswap

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.contract.vswap.{VSwapContractGen, VSwapFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.state._

class ExecuteVSwapInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with VSwapContractGen
  with VSwapFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVSwapDepositAndWithdrawLiquidityTokens: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, _, _, _, regLiquidity, regVSwap, _, _, issueLiquidity, _, _, depositLiquidity, fee, ts, attach) <- createABLiquidityTokenAndInitVSwap(1000, 1, 1000, 1000, 1, 1000,
      1000, 1000,10, 100, 100)
    withdrawLiquidity <- withdrawToken(master, regLiquidity.contractId, regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, 100L, fee, ts + 12)
    withdrawInvalidLiquidity <- withdrawToken(master, regLiquidity.contractId, regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, 2000L, fee, ts + 12)
  } yield (genesis, genesis2, regLiquidity, regVSwap, issueLiquidity, depositLiquidity, withdrawLiquidity, withdrawInvalidLiquidity)

  // withdraw liquidity tokens, withdraw token A and B have been local tested successfully
  property("withdraw liquidity tokens more than depositing in vswap contract") {
    forAll(preconditionsAndVSwapDepositAndWithdrawLiquidityTokens) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueLiquidity: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, withdrawLiquidity: ExecuteContractFunctionTransaction, withdrawInvalidLiquidity: ExecuteContractFunctionTransaction) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositLiquidity.timestamp, Seq(regLiquidity, regVSwap, issueLiquidity, depositLiquidity))),
        TestBlock.createWithTxStatus(withdrawLiquidity.timestamp, Seq(withdrawLiquidity), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositLiquidity.timestamp, Seq(regLiquidity, regVSwap, issueLiquidity, depositLiquidity))),
        TestBlock.createWithTxStatus(withdrawInvalidLiquidity.timestamp, Seq(withdrawInvalidLiquidity), TransactionStatus.ContractTokenBalanceInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  val preconditionsAndVSwapSetSwap: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, _, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(1000, 1, 1000, 1000, 1, 1000, 1000, 1000,9, 100, 100)

    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 10L, 10L, attach, fee + 10000000000L, ts)
    setInvalidSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 10L, attach, fee + 10000000000L, ts)
    setInvalidSwap2 <- setSwapVSwapGen(master, regVSwap.contractId, 8L, 10L, attach, fee + 10000000000L, ts)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, setInvalidSwap, setInvalidSwap2)

  property("set swap more than deposit") {
    forAll(preconditionsAndVSwapSetSwap) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, setInvalidSwap: ExecuteContractFunctionTransaction, setInvalidSwap2: ExecuteContractFunctionTransaction) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositLiquidity.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(setSwap), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // set swap token A more than deposit, set swap token B more than deposit has been local tested successfully
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositLiquidity.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(setInvalidSwap.timestamp, Seq(setInvalidSwap), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // set swap less than minimum liquidity (amountADesired), set swap less than minimum liquidity (amountBDesired) has been local tested successfully
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositLiquidity.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(setInvalidSwap2.timestamp, Seq(setInvalidSwap2), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVSwapAddLiquidity: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000, 10000, 1, 10000,
        10000, 1000,10, 3000, 3000)
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts)

    addLiquidity <- addLiquidityVSwapGen(master, regVSwap.contractId, 2000L, 2000L, 900L, 900L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    addInvalidLiquidity <- addLiquidityVSwapGen(master, regVSwap.contractId, 3000L, 2000L, 900L, 900L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    addInvalidLiquidity2 <- addLiquidityVSwapGen(master, regVSwap.contractId, 2000L, 2000L, 9000L, 900L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    addInvalidLiquidity3 <- addLiquidityVSwapGen(master, regVSwap.contractId, 2000L, 2000L, 900L, 900L, ts - 1000000000000L, attach, fee + 10000000000L, ts)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, addLiquidity, addInvalidLiquidity, addInvalidLiquidity2, addInvalidLiquidity3)

  property("unable to add liquidity") {
    forAll(preconditionsAndVSwapAddLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, addLiquidity: ExecuteContractFunctionTransaction,
    addInvalidLiquidity: ExecuteContractFunctionTransaction, addInvalidLiquidity2: ExecuteContractFunctionTransaction, addInvalidLiquidity3: ExecuteContractFunctionTransaction) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addLiquidity.timestamp, Seq(addLiquidity), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // add liquidity more than tokenADepositAmount, add liquidity more than tokenBDepositAmount has been locally tested successfully
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addInvalidLiquidity.timestamp, Seq(addInvalidLiquidity), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // add liquidity less than amountAMin, add liquidity less than amountBMin has been locally tested successfully
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addInvalidLiquidity2.timestamp, Seq(addInvalidLiquidity2), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // add liquidity with a wrong deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addInvalidLiquidity3.timestamp, Seq(addInvalidLiquidity3), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVSwapRemoveLiquidity: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, _, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000, 10000, 1,
        10000, 10000, 1000,10, 2000, 2000)
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts)

    removeLiquidity <- removeLiquidityVSwapGen(master, regVSwap.contractId, 100L, 100L, 100L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    removeInvalidLiquidity <- removeLiquidityVSwapGen(master, regVSwap.contractId, 2000L, 100L, 100L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    removeInvalidLiquidity2 <- removeLiquidityVSwapGen(master, regVSwap.contractId, 100L, 200L, 100L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    removeInvalidLiquidity3 <- removeLiquidityVSwapGen(master, regVSwap.contractId, 100L, 100L, 100L, ts - 1000000000000L, attach, fee + 10000000000L, ts)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, removeLiquidity, removeInvalidLiquidity, removeInvalidLiquidity2, removeInvalidLiquidity3)

  property("unable to remove liquidity") {
    forAll(preconditionsAndVSwapRemoveLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction,
    removeLiquidity: ExecuteContractFunctionTransaction, removeInvalidLiquidity: ExecuteContractFunctionTransaction, removeInvalidLiquidity2: ExecuteContractFunctionTransaction, removeInvalidLiquidity3: ExecuteContractFunctionTransaction) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeLiquidity.timestamp, Seq(removeLiquidity), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // remove liquidity more than in pool
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeInvalidLiquidity.timestamp, Seq(removeInvalidLiquidity), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // remove liquidity with wrong amountAMin, remove liquidity with wrong amountBMin has been local tested successfully
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeInvalidLiquidity2.timestamp, Seq(removeInvalidLiquidity2), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // remove liquidity with wrong removeLiquidityDeadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeInvalidLiquidity3.timestamp, Seq(removeInvalidLiquidity3), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVSwapSwapTokenForExactBaseToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, _, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000, 10000, 1, 10000,
        10000, 1000,10, 2000, 2000)
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts)

    swapTokenForExactBaseToken <- swapTokenForExactBaseTokenVSwapGen(master, regVSwap.contractId, 100L, 112L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    swapInvalidTokenForExactBaseToken <- swapTokenForExactBaseTokenVSwapGen(master, regVSwap.contractId, 100L, 111L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    swapInvalidTokenForExactBaseToken2 <- swapTokenForExactBaseTokenVSwapGen(master, regVSwap.contractId, 100L, 112L, ts - 1000000000000L, attach, fee + 10000000000L, ts)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapTokenForExactBaseToken, swapInvalidTokenForExactBaseToken, swapInvalidTokenForExactBaseToken2)

  property("unable to swap tokens for exact base token") {
    forAll(preconditionsAndVSwapSwapTokenForExactBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactBaseToken: ExecuteContractFunctionTransaction, swapInvalidTokenForExactBaseToken: ExecuteContractFunctionTransaction, swapInvalidTokenForExactBaseToken2: ExecuteContractFunctionTransaction) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapTokenForExactBaseToken), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // swap tokens for exact base token with wrong total supply
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidTokenForExactBaseToken.timestamp, Seq(swapInvalidTokenForExactBaseToken), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
        // swap tokens for exact base token with wrong swapDeadline
        assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
          regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
          TestBlock.createWithTxStatus(swapInvalidTokenForExactBaseToken2.timestamp, Seq(swapInvalidTokenForExactBaseToken2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
          blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
          blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
          blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
        }
      }
    }
  }

  val preconditionsAndVSwapSwapExactTokenForBaseToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, _, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000, 10000, 1, 10000,
        10000, 1000,10, 2000, 2000)
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts)

    swapExactTokenForBaseToken <- swapExactTokenForBaseTokenVSwapGen(master, regVSwap.contractId, 496, 990L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    swapInvalidExactTokenForBaseToken <- swapExactTokenForBaseTokenVSwapGen(master, regVSwap.contractId, 497, 990L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    swapInvalidExactTokenForBaseToken2 <- swapExactTokenForBaseTokenVSwapGen(master, regVSwap.contractId, 496, 990L, ts - 1000000000000L, attach, fee + 10000000000L, ts)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapExactTokenForBaseToken, swapInvalidExactTokenForBaseToken, swapInvalidExactTokenForBaseToken2)

  property("unable to swap exact token for base token") {
    forAll(preconditionsAndVSwapSwapExactTokenForBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForBaseToken: ExecuteContractFunctionTransaction,
    swapInvalidExactTokenForBaseToken: ExecuteContractFunctionTransaction, swapInvalidExactTokenForBaseToken2: ExecuteContractFunctionTransaction) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapExactTokenForBaseToken), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // swap exact token for base token with wrong total supply
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidExactTokenForBaseToken.timestamp, Seq(swapInvalidExactTokenForBaseToken), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // swap exact token for base token with wrong deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidExactTokenForBaseToken2.timestamp, Seq(swapInvalidExactTokenForBaseToken2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVSwapSwapTokenForExactTargetToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, _, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000, 10000, 1, 10000,
        10000, 1000,10, 2000, 2000)
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts)

    swapTokenForExactTargetToken <- swapTokenForExactTargetTokenVSwapGen(master, regVSwap.contractId, 100L, 112L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    swapInvalidTokenForExactTargetToken <- swapTokenForExactTargetTokenVSwapGen(master, regVSwap.contractId, 100L, 111L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    swapInvalidTokenForExactTargetToken2 <- swapTokenForExactTargetTokenVSwapGen(master, regVSwap.contractId, 100L, 112L, ts - 1000000000000L, attach, fee + 10000000000L, ts)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapTokenForExactTargetToken, swapInvalidTokenForExactTargetToken, swapInvalidTokenForExactTargetToken2)

  property("unable to swap token for exact target token") {
    forAll(preconditionsAndVSwapSwapTokenForExactTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactTargetToken: ExecuteContractFunctionTransaction,
    swapInvalidTokenForExactTargetToken: ExecuteContractFunctionTransaction, swapInvalidTokenForExactTargetToken2: ExecuteContractFunctionTransaction) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapTokenForExactTargetToken), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // swap token for exact target token with wrong total supply
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidTokenForExactTargetToken.timestamp, Seq(swapInvalidTokenForExactTargetToken), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // swap token for exact target token with wrong deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidTokenForExactTargetToken2.timestamp, Seq(swapInvalidTokenForExactTargetToken2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVSwapSwapExactTokenForTargetToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(10000, 1, 10000, 10000, 1, 10000,
        10000, 1000,10, 2000, 2000)
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts)

    swapExactTokenForTargetToken <- swapExactTokenForTargetTokenVSwapGen(master, regVSwap.contractId, 90L, 100L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    swapInvalidExactTokenForTargetToken <- swapExactTokenForTargetTokenVSwapGen(master, regVSwap.contractId, 91L, 100L, ts + 1000000000000L, attach, fee + 10000000000L, ts)

    swapInvalidExactTokenForTargetToken2 <- swapExactTokenForTargetTokenVSwapGen(master, regVSwap.contractId, 90L, 100L, ts - 1000000000000L, attach, fee + 10000000000L, ts)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapExactTokenForTargetToken, swapInvalidExactTokenForTargetToken, swapInvalidExactTokenForTargetToken2)

  property("unable to swap exact token for target token") {
    forAll(preconditionsAndVSwapSwapExactTokenForTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForTargetToken: ExecuteContractFunctionTransaction,
    swapInvalidExactTokenForTargetToken: ExecuteContractFunctionTransaction, swapInvalidExactTokenForTargetToken2: ExecuteContractFunctionTransaction) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapExactTokenForTargetToken), TransactionStatus.Success)) { (blockDiff) =>
        blockDiff.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // swap exact token for target token with wrong total supply
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidExactTokenForTargetToken.timestamp, Seq(swapInvalidExactTokenForTargetToken), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // swap exact token for target token with wrong deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(setSwap.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(swapInvalidExactTokenForTargetToken2.timestamp, Seq(swapInvalidExactTokenForTargetToken2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}