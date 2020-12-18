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

class ExecuteVSwapValidDiffTest extends PropSpec
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
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, fee, ts, attach) <- createABLiquidityTokenAndInitVSwap(1000, 1, 1000, 1000, 1, 1000,
      1000, 1000, 1000,10, 100, 100)
    //withdraw tokens
    withdrawTokenAData = Seq(regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(10))
    withdrawTokenAType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdrawTokenA <- withdrawTokenGen(master, regTokenA.contractId, false, withdrawTokenAData, withdrawTokenAType, attach, fee, ts + 10)

    withdrawTokenBData = Seq(regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(10))
    withdrawTokenBType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdrawTokenB <- withdrawTokenGen(master, regTokenB.contractId, false, withdrawTokenBData, withdrawTokenBType, attach, fee, ts + 11)

    withdrawLiquidityData = Seq(regVSwap.contractId.bytes.arr, master.toAddress.bytes.arr, Longs.toByteArray(10))
    withdrawLiquidityType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdrawLiquidity <- withdrawTokenGen(master, regLiquidity.contractId, false, withdrawLiquidityData, withdrawLiquidityType, attach, fee, ts + 12)
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity,
    withdrawTokenA, withdrawTokenB, withdrawLiquidity, fee, ts)

  property("vswap able to withdraw balance") {
    forAll(preconditionsAndVSwapDepositAndWithdrawLiquidityTokens) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, withdrawA: ExecuteContractFunctionTransaction, withdrawB: ExecuteContractFunctionTransaction,
    withdrawLiquidity: ExecuteContractFunctionTransaction,  _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(withdrawLiquidity.timestamp, Seq(withdrawA,withdrawB,withdrawLiquidity), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val master = withdrawLiquidity.proofs.firstCurveProof.explicitGet().publicKey
        val vswapContractId = regVSwap.contractId.bytes

        val tokenAContractId = regTokenA.contractId.bytes
        val tokenBContractId = regTokenB.contractId.bytes
        val liquidityContractId = regLiquidity.contractId.bytes

        val tokenAId = tokenIdFromBytes(tokenAContractId.arr, Ints.toByteArray(0)).explicitGet()
        val tokenBId = tokenIdFromBytes(tokenBContractId.arr, Ints.toByteArray(0)).explicitGet()
        val liquidityTokenId = tokenIdFromBytes(liquidityContractId.arr, Ints.toByteArray(0)).explicitGet()

        val contractTokenAIdBalanceKey = ByteStr(Bytes.concat(tokenAId.arr, vswapContractId.arr))
        val masterTokenAIdBalanceKey = ByteStr(Bytes.concat(tokenAId.arr, master.toAddress.bytes.arr))
        newState.tokenAccountBalance(masterTokenAIdBalanceKey) shouldBe 910L
        newState.tokenAccountBalance(contractTokenAIdBalanceKey) shouldBe 90L

        val contractTokenBBalanceKey = ByteStr(Bytes.concat(tokenBId.arr, vswapContractId.arr))
        val masterTokenBBalanceKey = ByteStr(Bytes.concat(tokenBId.arr, master.toAddress.bytes.arr))
        newState.tokenAccountBalance(masterTokenBBalanceKey) shouldBe 910L
        newState.tokenAccountBalance(contractTokenBBalanceKey) shouldBe 90L

        val contractLiquidityBalanceKey = ByteStr(Bytes.concat(liquidityTokenId.arr, vswapContractId.arr))
        val masterLiquidityBalanceKey = ByteStr(Bytes.concat(liquidityTokenId.arr, master.toAddress.bytes.arr))
        newState.tokenAccountBalance(masterLiquidityBalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractLiquidityBalanceKey) shouldBe 990L
      }
    }
  }

  val preconditionsAndVSwapSetSwap: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
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
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, fee, ts)

  property("vswap able to setswap") {
    forAll(preconditionsAndVSwapSetSwap) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction,  _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(setSwap), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = setSwap.proofs.firstCurveProof.explicitGet().publicKey
        val vswapContractId = regVSwap.contractId.bytes
        val tokenABalanceKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(0.toByte), DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes)) //tokenAReservedStateVar
        newState.contractNumInfo(tokenABalanceKey) shouldEqual 90L
        val tokenBBalanceKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(1.toByte), DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes)) //tokenAReservedStateVar
        newState.contractNumInfo(tokenBBalanceKey) shouldEqual 90L
        val tokenAReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(6.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 10L
        val tokenBReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(7.toByte))) //tokenBReservedStateVar
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 10L
      }
    }
  }

  val preconditionsAndVSwapAddLiquidity: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
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
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, addLiquidity, fee, ts)

  property("vswap able to add liquidity") {
    forAll(preconditionsAndVSwapAddLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, addLiquidity: ExecuteContractFunctionTransaction,  _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(addLiquidity.timestamp, Seq(addLiquidity), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val vswapContractId = regVSwap.contractId.bytes
        val tokenAReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(6.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 3000L
        val tokenBReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(7.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 3000L

        val master = addLiquidity.proofs.firstCurveProof.explicitGet().publicKey
        val tokenLiquidityBalanceKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(2.toByte), DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes)) //tokenAReservedStateVar
        newState.contractNumInfo(tokenLiquidityBalanceKey) shouldEqual 2990L
      }
    }
  }

  val preconditionsAndVSwapRemoveLiquidity: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
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
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, removeLiquidity, fee, ts)

  property("vswap able to remove liquidity") {
    forAll(preconditionsAndVSwapRemoveLiquidity) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction,
    removeLiquidity: ExecuteContractFunctionTransaction,  _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(removeLiquidity.timestamp, Seq(removeLiquidity), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val vswapContractId = regVSwap.contractId.bytes
        val tokenAReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(6.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 900L
        val tokenBReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(7.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 900L

        val tokenLiquidityLeftKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(9.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenLiquidityLeftKey) shouldEqual 9100L

        val master = removeLiquidity.proofs.firstCurveProof.explicitGet().publicKey
        val tokenLiquidityBalanceKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(2.toByte), DataEntry(master.toAddress.bytes.arr, DataType.Address).bytes)) //tokenAReservedStateVar
        newState.contractNumInfo(tokenLiquidityBalanceKey) shouldEqual 890L
      }
    }
  }

  val preconditionsAndVSwapSwapTokenForExactBaseToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
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
  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapTokenForExactBaseToken, fee, ts)

  property("vswap able to swap tokens for exact base token") {
    forAll(preconditionsAndVSwapSwapTokenForExactBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactBaseToken: ExecuteContractFunctionTransaction,  _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapTokenForExactBaseToken), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val vswapContractId = regVSwap.contractId.bytes
        val tokenAReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(6.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 900L
        val tokenBReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(7.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 1112L
      }
    }
  }


  val preconditionsAndVSwapSwapExactTokenForBaseToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
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
    swapExactTokenForBaseToken <- swapExactTokenForBaseTokenGen(master, regVSwap.contractId, swapData, swapDataType, attach, fee + 10000000000L, ts)

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapExactTokenForBaseToken, fee, ts)

  property("vswap able to swap exact token for base token") {
    forAll(preconditionsAndVSwapSwapExactTokenForBaseToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForBaseToken: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapExactTokenForBaseToken), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val vswapContractId = regVSwap.contractId.bytes
        val tokenAReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(6.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 910L
        val tokenBReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(7.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 1100L
      }
    }
  }

  val preconditionsAndVSwapSwapTokenForExactTargetToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
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

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapTokenForExactTargetToken, fee, ts)

  property("vswap able to swap token for exact target token") {
    forAll(preconditionsAndVSwapSwapTokenForExactTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapTokenForExactTargetToken: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapTokenForExactTargetToken), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val vswapContractId = regVSwap.contractId.bytes
        val tokenAReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(6.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 1112L
        val tokenBReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(7.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 900L
      }
    }
  }

  val preconditionsAndVSwapSwapExactTokenForTargetToken: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
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

  } yield (genesis, genesis2, regTokenA, regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA,
    depositB, depositLiquidity, setSwap, swapExactTokenForTargetToken, fee, ts)

  property("vswap able to swap exact token for target token") {
    forAll(preconditionsAndVSwapSwapExactTokenForTargetToken) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regTokenA: RegisterContractTransaction,
    regTokenB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVSwap: RegisterContractTransaction, issueTokenA: ExecuteContractFunctionTransaction,
    issueTokenB: ExecuteContractFunctionTransaction, issueLiquidity: ExecuteContractFunctionTransaction, depositA: ExecuteContractFunctionTransaction, depositB: ExecuteContractFunctionTransaction,
    depositLiquidity: ExecuteContractFunctionTransaction, setSwap: ExecuteContractFunctionTransaction, swapExactTokenForTargetToken: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regTokenA.timestamp, Seq(regTokenA,
        regTokenB, regLiquidity, regVSwap, issueTokenA, issueTokenB, issueLiquidity, depositA, depositB, depositLiquidity, setSwap))),
        TestBlock.createWithTxStatus(setSwap.timestamp, Seq(swapExactTokenForTargetToken), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val vswapContractId = regVSwap.contractId.bytes
        val tokenAReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(6.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 1100L
        val tokenBReservedKey = ByteStr(Bytes.concat(vswapContractId.arr, Array(7.toByte))) //tokenAReservedStateVar
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 910L
      }
    }
  }
}
