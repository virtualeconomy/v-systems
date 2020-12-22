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

class ExecuteVSwapValidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with VSwapContractGen
  with VSwapFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVSwapDepositAndWithdrawLiquidityTokens: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    // register 3 tokens and VSwap contract, then deposit all 3 tokens into contract
    // the function will issue and deposit the entire supply of liquidity tokens
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(
      1000, // total supply of token A
      1, // unity of token A
      1000, // issue amount of token A
      1000, // total supply of token B
      1, // unity of token B
      1000, // issue amount of token B
      1000, // total supply of liquidity token
      1000, // unity of liquidity token
      10, // minimum liquidity
      10, // deposit amount of token A
      10) // deposit amount of token B
    //withdraw tokens
    withdrawTokenA <- withdrawToken(master, regTokenA.contractId, regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, 10L, fee, ts + 10)
    withdrawTokenB <- withdrawToken(master, regTokenB.contractId, regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, 10L, fee, ts + 11)
    withdrawLiquidity <- withdrawToken(master, regLiquidity.contractId, regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, 10L, fee, ts + 12)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity,
    withdrawTokenA, withdrawTokenB, withdrawLiquidity)

  property("vswap able to withdraw balance") {
    forAll(preconditionsAndVSwapDepositAndWithdrawLiquidityTokens) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, withdrawA: ExecuteContractFunctionTransaction, withdrawB: ExecuteContractFunctionTransaction,
    withdrawLiquidity: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(withdrawLiquidity.timestamp, Seq(withdrawLiquidity), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVSwapSetSwap: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(
        1000, // totalSupplyA
        1, // unityA
        1000, // issueAmountA
        1000, // totalSupplyB
        1, //unityB
        1000, // issueAmountB
        1000, // liquidityTotalSupply
        1000, // liquidityUnity
        9, // minimumLiquidity
        100, // tokenADepositAmount
        100) // tokenBDepositAmount
    // set swap
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 10L, 10L, attach, fee + 10000000000L, ts + 10)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap)

  property("vswap able to setswap") {
    forAll(preconditionsAndVSwapSetSwap) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositLiquidity.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(setSwap), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVSwapAddLiquidity: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(
        10000, // totalSupplyA
        1, // unityA
        10000, // issueAmountA
        10000, // totalSupplyB
        1, // unityB
        10000, // issueAmountB
        10000, // liquidityTotalSupply
        1000, // liquidityUnity
        10, // minimumLiquidity
        2000, // tokenADepositAmount
        2000) // tokenBDepositAmount

    // set swap -> amountADesired, amountBDesired
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts + 10)

    // add liquidity -> amountADesired, amountBDesired, amountAMin, amountBMin
    addLiquidity <- addLiquidityVSwapGen(master, regVSwap.contractId, 1000L, 1000L, 900L, 900L, ts + 1000000000000L, attach, fee + 10000000000L, ts + 11)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, addLiquidity)

  property("vswap able to add liquidity") {
    forAll(preconditionsAndVSwapAddLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, addLiquidity: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(addLiquidity), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVSwapRemoveLiquidity: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(
        10000, // totalSupplyA
        1, // unityA
        10000, // issueAmountA
        10000, // totalSupplyB
        1, // unityB
        10000, // issueAmountB
        10000, // liquidityTotalSupply
        1000, // liquidityUnity
        10, // minimumLiquidity
        2000, // tokenADepositAmount
        2000) // tokenBDepositAmount

    // set swap -> amountADesired, amountBDesired
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts + 10)

    // add liquidity -> amountADesired, amountBDesired, amountAMin, amountBMin
    addLiquidity <- addLiquidityVSwapGen(master, regVSwap.contractId, 1000L, 1000L, 900L, 900L, ts + 1000000000000L, attach, fee + 10000000000L, ts + 11)

    // remove liquidity -> liquidity, amountAMin, amountBMin
    removeLiquidity <- removeLiquidityVSwapGen(master, regVSwap.contractId, 1000L, 1000L, 1000L, ts + 1000000000000L, attach, fee + 10000000000L, ts + 12)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, addLiquidity, removeLiquidity)

  property("vswap able to remove liquidity") {
    forAll(preconditionsAndVSwapRemoveLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, addLiquidity: ExecuteContractFunctionTransaction,
    removeLiquidity: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap, addLiquidity))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(removeLiquidity), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVSwapSwapTokenForExactBaseToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(
        10000, // totalSupplyA
        1, // unityA
        10000, // issueAmountA
        10000, // totalSupplyB
        1, // unityB
        10000, // issueAmountB
        10000, // liquidityTotalSupply
        1000, // liquidityUnity
        10, // minimumLiquidity
        2000, // tokenADepositAmount
        2000) // tokenBDepositAmount

    // set swap -> amountADesired, amountBDesired
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts + 10)

    // swap for 100 base tokens -> amountOut, amountInMax, deadline
    swapTokenForExactBaseToken <- swapTokenForExactBaseTokenVSwapGen(master, regVSwap.contractId, 100L, 112L, ts + 1000000000000L, attach, fee + 10000000000L, ts + 11)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapTokenForExactBaseToken)

  property("vswap able to swap tokens for exact base token") {
    forAll(preconditionsAndVSwapSwapTokenForExactBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactBaseToken: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapTokenForExactBaseToken), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVSwapSwapExactTokenForBaseToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(
        10000, // totalSupplyA
        1, // unityA
        10000, // issueAmountA
        10000, // totalSupplyB
        1, // unityB
        10000, // issueAmountB
        10000, // liquidityTotalSupply
        1000, // liquidityUnity
        10, // minimumLiquidity
        2000, // tokenADepositAmount
        2000) // tokenBDepositAmount

    // set swap -> amountADesired, amountBDesired
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts + 10)

    // swap 100 tokens for base tokens -> amountOutMin, amountIn, deadline
    swapExactTokenForBaseToken <- swapExactTokenForBaseTokenVSwapGen(master, regVSwap.contractId, 90L, 100L, ts + 1000000000000L, attach, fee + 10000000000L, ts + 11)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapExactTokenForBaseToken)

  property("vswap able to swap exact token for base token") {
    forAll(preconditionsAndVSwapSwapExactTokenForBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForBaseToken: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapExactTokenForBaseToken), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVSwapSwapTokenForExactTargetToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(
        10000, // totalSupplyA
        1, // unityA
        10000, // issueAmountA
        10000, // totalSupplyB
        1, // unityB
        10000, // issueAmountB
        10000, // liquidityTotalSupply
        1000, // liquidityUnity
        10, // minimumLiquidity
        2000, // tokenADepositAmount
        2000) // tokenBDepositAmount

    // set swap -> amountADesired, amountBDesired
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts + 10)

    // swap for 100 target tokens
    swapTokenForExactTargetToken <- swapTokenForExactTargetTokenVSwapGen(master, regVSwap.contractId, 100L, 112L, ts + 1000000000000L, attach, fee + 10000000000L, ts + 11)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapTokenForExactTargetToken)

  property("vswap able to swap token for exact target token") {
    forAll(preconditionsAndVSwapSwapTokenForExactTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactTargetToken: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapTokenForExactTargetToken), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVSwapSwapExactTokenForTargetToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <-
      createABLiquidityTokenAndInitVSwap(
        10000, // totalSupplyA
        1, // unityA
        10000, // issueAmountA
        10000, // totalSupplyB
        1, // unityB
        10000, // issueAmountB
        10000, // liquidiityTotalSupply
        1000, // liquidityUnity
        10, // minimumLiquidity
        2000, // tokenADepositAmount
        2000) // tokenBDepositAmount

    // set swap -> amountADesired, amountBDesired
    setSwap <- setSwapVSwapGen(master, regVSwap.contractId, 1000L, 1000L, attach, fee + 10000000000L, ts + 10)

    // swap 100 tokens for target token -> amountOutMin, amountIn
    swapExactTokenForTargetToken <- swapExactTokenForTargetTokenVSwapGen(master, regVSwap.contractId, 90L, 100L, ts + 1000000000000L, attach, fee + 10000000000L, ts + 11)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapExactTokenForTargetToken)

  property("vswap able to swap exact token for target token") {
    forAll(preconditionsAndVSwapSwapExactTokenForTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForTargetToken: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapExactTokenForTargetToken), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }
}