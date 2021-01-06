package vsys.blockchain.state.contract.vswap

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.contract.vswap.{VSwapContractGen, VSwapFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.account.PublicKeyAccount

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

  def getContractTokenBalanceKeys(tokenAContractId: Array[Byte], tokenBContractId: Array[Byte], liquidityContractId: Array[Byte], vSwapContractId: Array[Byte]): (ByteStr, ByteStr, ByteStr) = {
    val tokenAId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()
    val tokenBId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet()
    val liquidityTokenId = tokenIdFromBytes(liquidityContractId, Ints.toByteArray(0)).explicitGet()

    val contractTokenABalanceKey = ByteStr(Bytes.concat(tokenAId.arr, vSwapContractId))
    val contractTokenBBalanceKey = ByteStr(Bytes.concat(tokenBId.arr, vSwapContractId))
    val contractLiquidityBalanceKey = ByteStr(Bytes.concat(liquidityTokenId.arr, vSwapContractId))

    (contractTokenABalanceKey, contractTokenBBalanceKey, contractLiquidityBalanceKey)
  }

  def getUserTokenBalanceKeys(tokenAContractId: Array[Byte], tokenBContractId: Array[Byte], liquidityContractId: Array[Byte], user: PublicKeyAccount): (ByteStr, ByteStr, ByteStr) = {
    val tokenAId = tokenIdFromBytes(tokenAContractId, Ints.toByteArray(0)).explicitGet()
    val tokenBId = tokenIdFromBytes(tokenBContractId, Ints.toByteArray(0)).explicitGet()
    val liquidityTokenId = tokenIdFromBytes(liquidityContractId, Ints.toByteArray(0)).explicitGet()

    val masterTokenABalanceKey = ByteStr(Bytes.concat(tokenAId.arr, user.toAddress.bytes.arr))
    val masterTokenBBalanceKey = ByteStr(Bytes.concat(tokenBId.arr, user.toAddress.bytes.arr))
    val masterLiquidityBalanceKey = ByteStr(Bytes.concat(liquidityTokenId.arr, user.toAddress.bytes.arr))

    (masterTokenABalanceKey, masterTokenBBalanceKey, masterLiquidityBalanceKey)
  }

  def getSwapContractStateVarKeys(vSwapContractId: Array[Byte]): (ByteStr, ByteStr, ByteStr, ByteStr, ByteStr, ByteStr) = {
    val swapStatusKey = ByteStr(Bytes.concat(vSwapContractId, Array(4.toByte)))
    val minimumLiquidityKey = ByteStr(Bytes.concat(vSwapContractId, Array(5.toByte)))
    val tokenAReservedKey = ByteStr(Bytes.concat(vSwapContractId, Array(6.toByte)))
    val tokenBReservedKey = ByteStr(Bytes.concat(vSwapContractId, Array(7.toByte)))
    val totalSupplyKey = ByteStr(Bytes.concat(vSwapContractId, Array(8.toByte)))
    val liquidityTokenLeft = ByteStr(Bytes.concat(vSwapContractId, Array(9.toByte)))

    (swapStatusKey, minimumLiquidityKey, tokenAReservedKey, tokenBReservedKey, totalSupplyKey, liquidityTokenLeft)
  }

  def getSwapContractStateMapKeys(vSwapContractId: Array[Byte], user: PublicKeyAccount): (ByteStr, ByteStr, ByteStr) = {
    val stateMapTokenABalanceKey = ByteStr(Bytes.concat(vSwapContractId, Array(0.toByte), DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes))
    val stateMapTokenBBalanceKey = ByteStr(Bytes.concat(vSwapContractId, Array(1.toByte), DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes))
    val stateMapLiquidityTokenBalanceKey = ByteStr(Bytes.concat(vSwapContractId, Array(2.toByte), DataEntry(user.toAddress.bytes.arr, DataType.Address).bytes))

    (stateMapTokenABalanceKey, stateMapTokenBBalanceKey, stateMapLiquidityTokenBalanceKey)
  }

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
      1000, // deposit amount of token A
      1000) // deposit amount of token B
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
        TestBlock.createWithTxStatus(withdrawLiquidity.timestamp, Seq(withdrawA, withdrawB, withdrawLiquidity), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = withdrawLiquidity.proofs.firstCurveProof.explicitGet().publicKey
        val vSwapContractId = regVSwap.contractId.bytes.arr

        val tokenAContractId = regTokenA.contractId.bytes.arr
        val tokenBContractId = regTokenB.contractId.bytes.arr
        val liquidityContractId = regLiquidity.contractId.bytes.arr

        val (contractTokenABalanceKey, contractTokenBBalanceKey, contractLiquidityBalanceKey) = getContractTokenBalanceKeys(tokenAContractId, tokenBContractId, liquidityContractId, vSwapContractId)
        val (masterTokenABalanceKey, masterTokenBBalanceKey, masterLiquidityBalanceKey) = getUserTokenBalanceKeys(tokenAContractId, tokenBContractId, liquidityContractId, master)

        newState.tokenAccountBalance(masterTokenABalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractTokenABalanceKey) shouldBe 990L

        newState.tokenAccountBalance(masterTokenBBalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractTokenBBalanceKey) shouldBe 990L

        newState.tokenAccountBalance(masterLiquidityBalanceKey) shouldBe 10L
        newState.tokenAccountBalance(contractLiquidityBalanceKey) shouldBe 990L
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

        val master = setSwap.proofs.firstCurveProof.explicitGet().publicKey
        val vSwapContractId = regVSwap.contractId.bytes.arr

        val (swapStatusKey, minimumLiquidityKey, tokenAReservedKey, tokenBReservedKey, totalSupplyKey, liquidityTokenLeft) = getSwapContractStateVarKeys(vSwapContractId)
        val (masterStateMapTokenABalanceKey, masterStateMapTokenBBalanceKey, masterStateMapLiquidityTokenBalanceKey) = getSwapContractStateMapKeys(vSwapContractId, master)

        newState.contractInfo(swapStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(minimumLiquidityKey) shouldEqual Some(DataEntry(Longs.toByteArray(9), DataType.Amount))
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 10L
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 10L
        newState.contractNumInfo(totalSupplyKey) shouldEqual 10L // sqrt(10 * 10)
        newState.contractNumInfo(liquidityTokenLeft) shouldEqual 990L // 1000 - sqrt(10*10)

        newState.contractNumInfo(masterStateMapTokenABalanceKey) shouldEqual 90L
        newState.contractNumInfo(masterStateMapTokenBBalanceKey) shouldEqual 90L
        newState.contractNumInfo(masterStateMapLiquidityTokenBalanceKey) shouldEqual 1L // sqrt(10 * 10) - 9



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

        val master = setSwap.proofs.firstCurveProof.explicitGet().publicKey
        val vSwapContractId = regVSwap.contractId.bytes.arr

        val (swapStatusKey, minimumLiquidityKey, tokenAReservedKey, tokenBReservedKey, totalSupplyKey, liquidityTokenLeft) = getSwapContractStateVarKeys(vSwapContractId)
        val (masterStateMapTokenABalanceKey, masterStateMapTokenBBalanceKey, masterStateMapLiquidityTokenBalanceKey) = getSwapContractStateMapKeys(vSwapContractId, master)

        newState.contractInfo(swapStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(minimumLiquidityKey) shouldEqual Some(DataEntry(Longs.toByteArray(10), DataType.Amount))
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 2000L
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 2000L
        newState.contractNumInfo(totalSupplyKey) shouldEqual 2000L
        newState.contractNumInfo(liquidityTokenLeft) shouldEqual 8000L

        newState.contractNumInfo(masterStateMapTokenABalanceKey) shouldEqual 0L
        newState.contractNumInfo(masterStateMapTokenBBalanceKey) shouldEqual 0L
        newState.contractNumInfo(masterStateMapLiquidityTokenBalanceKey) shouldEqual 1990L
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

        val master = setSwap.proofs.firstCurveProof.explicitGet().publicKey
        val vSwapContractId = regVSwap.contractId.bytes.arr

        val (swapStatusKey, minimumLiquidityKey, tokenAReservedKey, tokenBReservedKey, totalSupplyKey, liquidityTokenLeft) = getSwapContractStateVarKeys(vSwapContractId)
        val (masterStateMapTokenABalanceKey, masterStateMapTokenBBalanceKey, masterStateMapLiquidityTokenBalanceKey) = getSwapContractStateMapKeys(vSwapContractId, master)

        newState.contractInfo(swapStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(minimumLiquidityKey) shouldEqual Some(DataEntry(Longs.toByteArray(10), DataType.Amount))
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 1000L
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 1000L
        newState.contractNumInfo(totalSupplyKey) shouldEqual 1000L
        newState.contractNumInfo(liquidityTokenLeft) shouldEqual 9000L

        newState.contractNumInfo(masterStateMapTokenABalanceKey) shouldEqual 1000L
        newState.contractNumInfo(masterStateMapTokenBBalanceKey) shouldEqual 1000L
        newState.contractNumInfo(masterStateMapLiquidityTokenBalanceKey) shouldEqual 990L
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

        val master = setSwap.proofs.firstCurveProof.explicitGet().publicKey
        val vSwapContractId = regVSwap.contractId.bytes.arr

        val (swapStatusKey, minimumLiquidityKey, tokenAReservedKey, tokenBReservedKey, totalSupplyKey, liquidityTokenLeft) = getSwapContractStateVarKeys(vSwapContractId)
        val (masterStateMapTokenABalanceKey, masterStateMapTokenBBalanceKey, masterStateMapLiquidityTokenBalanceKey) = getSwapContractStateMapKeys(vSwapContractId, master)

        newState.contractInfo(swapStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(minimumLiquidityKey) shouldEqual Some(DataEntry(Longs.toByteArray(10), DataType.Amount))
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 900L
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 1112L
        newState.contractNumInfo(totalSupplyKey) shouldEqual 1000L
        newState.contractNumInfo(liquidityTokenLeft) shouldEqual 9000L

        newState.contractNumInfo(masterStateMapTokenABalanceKey) shouldEqual 1100L
        newState.contractNumInfo(masterStateMapTokenBBalanceKey) shouldEqual 888L
        newState.contractNumInfo(masterStateMapLiquidityTokenBalanceKey) shouldEqual 990L
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

        val master = setSwap.proofs.firstCurveProof.explicitGet().publicKey
        val vSwapContractId = regVSwap.contractId.bytes.arr

        val (swapStatusKey, minimumLiquidityKey, tokenAReservedKey, tokenBReservedKey, totalSupplyKey, liquidityTokenLeft) = getSwapContractStateVarKeys(vSwapContractId)
        val (masterStateMapTokenABalanceKey, masterStateMapTokenBBalanceKey, masterStateMapLiquidityTokenBalanceKey) = getSwapContractStateMapKeys(vSwapContractId, master)

        newState.contractInfo(swapStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(minimumLiquidityKey) shouldEqual Some(DataEntry(Longs.toByteArray(10), DataType.Amount))
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 910L
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 1100L
        newState.contractNumInfo(totalSupplyKey) shouldEqual 1000L
        newState.contractNumInfo(liquidityTokenLeft) shouldEqual 9000L

        newState.contractNumInfo(masterStateMapTokenABalanceKey) shouldEqual 1090L
        newState.contractNumInfo(masterStateMapTokenBBalanceKey) shouldEqual 900L
        newState.contractNumInfo(masterStateMapLiquidityTokenBalanceKey) shouldEqual 990L
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

        val master = setSwap.proofs.firstCurveProof.explicitGet().publicKey
        val vSwapContractId = regVSwap.contractId.bytes.arr

        val (swapStatusKey, minimumLiquidityKey, tokenAReservedKey, tokenBReservedKey, totalSupplyKey, liquidityTokenLeft) = getSwapContractStateVarKeys(vSwapContractId)
        val (masterStateMapTokenABalanceKey, masterStateMapTokenBBalanceKey, masterStateMapLiquidityTokenBalanceKey) = getSwapContractStateMapKeys(vSwapContractId, master)

        newState.contractInfo(swapStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(minimumLiquidityKey) shouldEqual Some(DataEntry(Longs.toByteArray(10), DataType.Amount))
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 1112L
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 900L
        newState.contractNumInfo(totalSupplyKey) shouldEqual 1000L
        newState.contractNumInfo(liquidityTokenLeft) shouldEqual 9000L

        newState.contractNumInfo(masterStateMapTokenABalanceKey) shouldEqual 888L
        newState.contractNumInfo(masterStateMapTokenBBalanceKey) shouldEqual 1100L
        newState.contractNumInfo(masterStateMapLiquidityTokenBalanceKey) shouldEqual 990L
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

        val master = setSwap.proofs.firstCurveProof.explicitGet().publicKey
        val vSwapContractId = regVSwap.contractId.bytes.arr

        val (swapStatusKey, minimumLiquidityKey, tokenAReservedKey, tokenBReservedKey, totalSupplyKey, liquidityTokenLeft) = getSwapContractStateVarKeys(vSwapContractId)
        val (masterStateMapTokenABalanceKey, masterStateMapTokenBBalanceKey, masterStateMapLiquidityTokenBalanceKey) = getSwapContractStateMapKeys(vSwapContractId, master)

        newState.contractInfo(swapStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(minimumLiquidityKey) shouldEqual Some(DataEntry(Longs.toByteArray(10), DataType.Amount))
        newState.contractNumInfo(tokenAReservedKey) shouldEqual 1100L
        newState.contractNumInfo(tokenBReservedKey) shouldEqual 910L
        newState.contractNumInfo(totalSupplyKey) shouldEqual 1000L
        newState.contractNumInfo(liquidityTokenLeft) shouldEqual 9000L

        newState.contractNumInfo(masterStateMapTokenABalanceKey) shouldEqual 900L
        newState.contractNumInfo(masterStateMapTokenBBalanceKey) shouldEqual 1090L
        newState.contractNumInfo(masterStateMapLiquidityTokenBalanceKey) shouldEqual 990L
      }
    }
  }
}