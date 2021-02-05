package vsys.blockchain.state.contract.vstableswap

import com.google.common.primitives.{Longs, Ints}
import vsys.account.ContractAccount.tokenIdFromBytes
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.token.SystemContractGen
import vsys.blockchain.contract.vstableswap.{VStableSwapContractGen, VStableSwapFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.state._
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction => EC, RegisterContractTransaction => RC}
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}

class ExecuteVStableSwapValidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with VStableSwapContractGen
  with VStableSwapFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVStableSwapDepositToken: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC)] = for {
    (genesis, _, _, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, _, _, _)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unitPriceBase
      1) // unitPriceTarget
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget)

  property("V Stable Swap able to deposit") {
    forAll(preconditionsAndVStableSwapDepositToken) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase))),
        TestBlock.createWithTxStatus(depositBase.timestamp, Seq(depositBase), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val (contractTokenBaseBalanceKey, _) = getContractTokenBalanceKeys(regBase.contractId.bytes.arr, regTarget.contractId.bytes.arr,
          regVStableSwap.contractId.bytes.arr)

        newState.tokenAccountBalance(contractTokenBaseBalanceKey) shouldEqual 1000L

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (masterTokenBaseBalanceKey, _) = getUserTokenBalanceKeys(regBase.contractId.bytes.arr, regTarget.contractId.bytes.arr, master)

        newState.tokenAccountBalance(masterTokenBaseBalanceKey) shouldEqual 0L
      }
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueTarget))),
        TestBlock.createWithTxStatus(depositBase.timestamp, Seq(depositTarget), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val (_, contractTokenTargetBalanceKey) = getContractTokenBalanceKeys(regBase.contractId.bytes.arr, regTarget.contractId.bytes.arr,
          regVStableSwap.contractId.bytes.arr)

        newState.tokenAccountBalance(contractTokenTargetBalanceKey) shouldEqual 1000L

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (_, masterTokenTargetBalanceKey) = getUserTokenBalanceKeys(regBase.contractId.bytes.arr, regTarget.contractId.bytes.arr, master)

        newState.tokenAccountBalance(masterTokenTargetBalanceKey) shouldEqual 0L
      }
    }
  }

  val preconditionsAndVStableSwapWithdrawToken: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, _)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unitPriceBase
      1) // unitPriceTarget
    withdrawBase <- withdrawToken(master, regTokenBase.contractId, regVStableSwapContract.contractId.bytes.arr, master.toAddress.bytes.arr, 500L, fee, ts + 7)
    withdrawTarget <- withdrawToken(master, regTokenTarget.contractId, regVStableSwapContract.contractId.bytes.arr, master.toAddress.bytes.arr, 500L, fee, ts + 8)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, withdrawBase, withdrawTarget)

  property("V Stable Swap able to withdraw") {
    forAll(preconditionsAndVStableSwapWithdrawToken) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, withdrawBase: EC, withdrawTarget: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositBase.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, depositBase))),
        TestBlock.createWithTxStatus(withdrawBase.timestamp, Seq(withdrawBase), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val (contractTokenBaseBalanceKey, _) = getContractTokenBalanceKeys(regBase.contractId.bytes.arr, regTarget.contractId.bytes.arr,
          regVStableSwap.contractId.bytes.arr)

        newState.tokenAccountBalance(contractTokenBaseBalanceKey) shouldEqual 500L

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (masterTokenBaseBalanceKey, _) = getUserTokenBalanceKeys(regBase.contractId.bytes.arr, regTarget.contractId.bytes.arr, master)

        newState.tokenAccountBalance(masterTokenBaseBalanceKey) shouldEqual 500L
      }
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueTarget, depositTarget))),
        TestBlock.createWithTxStatus(withdrawTarget.timestamp, Seq(withdrawTarget), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val (_, contractTokenTargetBalanceKey) = getContractTokenBalanceKeys(regBase.contractId.bytes.arr, regTarget.contractId.bytes.arr,
          regVStableSwap.contractId.bytes.arr)

        newState.tokenAccountBalance(contractTokenTargetBalanceKey) shouldEqual 500L

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (_, masterTokenTargetBalanceKey) = getUserTokenBalanceKeys(regBase.contractId.bytes.arr, regTarget.contractId.bytes.arr, master)

        newState.tokenAccountBalance(masterTokenTargetBalanceKey) shouldEqual 500L
      }
    }
  }

  val preconditionsAndVStableSwapSupersedeAndSetOrder: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, user, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unitPriceBase
      1) // unitPriceTarget
    supersede <- supersedeVStableSwapGen(master, regVStableSwapContract.contractId, user.toAddress, attach, fee, ts + 5)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 1, 1, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, supersede, setOrder)

  property("V Stable Swap able to supersede and set order") {
    forAll(preconditionsAndVStableSwapSupersedeAndSetOrder) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, supersede: EC, setOrder: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositBase.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, supersede))),
        TestBlock.createWithTxStatus(setOrder.timestamp, Seq(setOrder), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (_, baseTokenIdKey, targetTokenIdKey, maxOrderPerUserKey, unitPriceBaseKey, unitPriceTargetKey) = getStableSwapContractStateVarKeys(regVStableSwap.contractId.bytes.arr)

        newState.contractInfo(baseTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regBase.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(targetTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regTarget.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(maxOrderPerUserKey) shouldBe Some(DataEntry(Longs.toByteArray(5), DataType.Amount))
        newState.contractInfo(unitPriceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))
        newState.contractInfo(unitPriceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))

        val (baseTokenBalanceKey, targetTokenBalanceKey, userOrdersKey, orderOwnerKey, feeBaseKey, feeTargetKey, minBaseKey, maxBaseKey,
          minTargetKey, maxTargetKey, priceBaseKey, priceTargetKey, baseTokenLockedKey, targetTokenLockedKey, orderStatusKey) = getStableSwapContractStateMapKeys(regVStableSwap.contractId.bytes.arr, setOrder.id.arr, master)

        newState.contractNumInfo(baseTokenBalanceKey) shouldBe 500L
        newState.contractNumInfo(targetTokenBalanceKey) shouldBe 500L
        newState.contractNumInfo(userOrdersKey) shouldBe 1L
        newState.contractInfo(orderOwnerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(feeBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(feeTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(minBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(minTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(priceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(priceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractNumInfo(baseTokenLockedKey) shouldBe 500L
        newState.contractNumInfo(targetTokenLockedKey) shouldBe 500L
        newState.contractInfo(orderStatusKey) shouldBe Some(DataEntry(Array(1.toByte), DataType.Boolean))
      }
    }
  }

  val preconditionsAndVStableSwapSetAndUpdateOrder: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unitPriceBase
      1) // unitPriceTarget
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    updateOrder <- updateVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10, 10, 10, 100, 10, 100, 5, 5, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, updateOrder)

  property("V Stable Swap able to set and update orders") {
    forAll(preconditionsAndVStableSwapSetAndUpdateOrder) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, updateOrder: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(updateOrder.timestamp, Seq(updateOrder), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (makerKey, baseTokenIdKey, targetTokenIdKey, maxOrderPerUserKey, unitPriceBaseKey, unitPriceTargetKey) = getStableSwapContractStateVarKeys(regVStableSwap.contractId.bytes.arr)

        newState.contractInfo(makerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(baseTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regBase.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(targetTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regTarget.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(maxOrderPerUserKey) shouldBe Some(DataEntry(Longs.toByteArray(5), DataType.Amount))
        newState.contractInfo(unitPriceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))
        newState.contractInfo(unitPriceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))

        val (baseTokenBalanceKey, targetTokenBalanceKey, userOrdersKey, orderOwnerKey, feeBaseKey, feeTargetKey, minBaseKey, maxBaseKey,
        minTargetKey, maxTargetKey, priceBaseKey, priceTargetKey, baseTokenLockedKey, targetTokenLockedKey, orderStatusKey) = getStableSwapContractStateMapKeys(regVStableSwap.contractId.bytes.arr, setOrder.id.arr, master)

        newState.contractNumInfo(baseTokenBalanceKey) shouldBe 500L
        newState.contractNumInfo(targetTokenBalanceKey) shouldBe 500L
        newState.contractNumInfo(userOrdersKey) shouldBe 1L
        newState.contractInfo(orderOwnerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(feeBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(10L), DataType.Amount))
        newState.contractInfo(feeTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(10L), DataType.Amount))
        newState.contractInfo(minBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(10L), DataType.Amount))
        newState.contractInfo(maxBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(minTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(10L), DataType.Amount))
        newState.contractInfo(maxTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(priceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(5L), DataType.Amount))
        newState.contractInfo(priceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(5L), DataType.Amount))
        newState.contractNumInfo(baseTokenLockedKey) shouldBe 500L
        newState.contractNumInfo(targetTokenLockedKey) shouldBe 500L
        newState.contractInfo(orderStatusKey) shouldBe Some(DataEntry(Array(1.toByte), DataType.Boolean))
      }
    }
  }

  val preconditionsAndVStableSwapSetAndOrderDeposit: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unitPriceBase
      1) // unitPriceTarget
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 5, 5, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    orderDeposit <- orderDepositVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 100, 100, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, orderDeposit)

  property("V Stable Swap able to set and deposit to orders") {
    forAll(preconditionsAndVStableSwapSetAndOrderDeposit) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, orderDeposit: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderDeposit.timestamp, Seq(orderDeposit), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (makerKey, baseTokenIdKey, targetTokenIdKey, maxOrderPerUserKey, unitPriceBaseKey, unitPriceTargetKey) = getStableSwapContractStateVarKeys(regVStableSwap.contractId.bytes.arr)

        newState.contractInfo(makerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(baseTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regBase.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(targetTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regTarget.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(maxOrderPerUserKey) shouldBe Some(DataEntry(Longs.toByteArray(5), DataType.Amount))
        newState.contractInfo(unitPriceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))
        newState.contractInfo(unitPriceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))

        val (baseTokenBalanceKey, targetTokenBalanceKey, userOrdersKey, orderOwnerKey, feeBaseKey, feeTargetKey, minBaseKey, maxBaseKey,
        minTargetKey, maxTargetKey, priceBaseKey, priceTargetKey, baseTokenLockedKey, targetTokenLockedKey, orderStatusKey) = getStableSwapContractStateMapKeys(regVStableSwap.contractId.bytes.arr, setOrder.id.arr, master)

        newState.contractNumInfo(baseTokenBalanceKey) shouldBe 400L
        newState.contractNumInfo(targetTokenBalanceKey) shouldBe 400L
        newState.contractNumInfo(userOrdersKey) shouldBe 1L
        newState.contractInfo(orderOwnerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(feeBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(5L), DataType.Amount))
        newState.contractInfo(feeTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(5L), DataType.Amount))
        newState.contractInfo(minBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(minTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(priceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(priceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractNumInfo(baseTokenLockedKey) shouldBe 600L
        newState.contractNumInfo(targetTokenLockedKey) shouldBe 600L
        newState.contractInfo(orderStatusKey) shouldBe Some(DataEntry(Array(1.toByte), DataType.Boolean))
      }
    }
  }

  val preconditionsAndVStableSwapSetAndOrderWithdraw: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unitPriceBase
      1) // unitPriceTarget
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 5, 5, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    orderWithdraw <- orderWithdrawVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 100, 100, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, orderWithdraw)

  property("V Stable Swap able to set and withdraw from orders") {
    forAll(preconditionsAndVStableSwapSetAndOrderWithdraw) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, orderWithdraw: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderWithdraw.timestamp, Seq(orderWithdraw), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (makerKey, baseTokenIdKey, targetTokenIdKey, maxOrderPerUserKey, unitPriceBaseKey, unitPriceTargetKey) = getStableSwapContractStateVarKeys(regVStableSwap.contractId.bytes.arr)

        newState.contractInfo(makerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(baseTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regBase.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(targetTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regTarget.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(maxOrderPerUserKey) shouldBe Some(DataEntry(Longs.toByteArray(5), DataType.Amount))
        newState.contractInfo(unitPriceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))
        newState.contractInfo(unitPriceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))

        val (baseTokenBalanceKey, targetTokenBalanceKey, userOrdersKey, orderOwnerKey, feeBaseKey, feeTargetKey, minBaseKey, maxBaseKey,
        minTargetKey, maxTargetKey, priceBaseKey, priceTargetKey, baseTokenLockedKey, targetTokenLockedKey, orderStatusKey) = getStableSwapContractStateMapKeys(regVStableSwap.contractId.bytes.arr, setOrder.id.arr, master)

        newState.contractNumInfo(baseTokenBalanceKey) shouldBe 600L
        newState.contractNumInfo(targetTokenBalanceKey) shouldBe 600L
        newState.contractNumInfo(userOrdersKey) shouldBe 1L
        newState.contractInfo(orderOwnerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(feeBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(5L), DataType.Amount))
        newState.contractInfo(feeTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(5L), DataType.Amount))
        newState.contractInfo(minBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(minTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(priceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(priceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractNumInfo(baseTokenLockedKey) shouldBe 400L
        newState.contractNumInfo(targetTokenLockedKey) shouldBe 400L
        newState.contractInfo(orderStatusKey) shouldBe Some(DataEntry(Array(1.toByte), DataType.Boolean))
      }
    }
  }

  val preconditionsAndVStableSwapSetAndOrderClose: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unitPriceBase
      1) // unitPriceTarget
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 1, 1, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    orderClose <- closeVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, orderClose)

  property("V Stable Swap able to close orders") {
    forAll(preconditionsAndVStableSwapSetAndOrderClose) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, orderClose: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderClose.timestamp, Seq(orderClose), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (makerKey, baseTokenIdKey, targetTokenIdKey, maxOrderPerUserKey, unitPriceBaseKey, unitPriceTargetKey) = getStableSwapContractStateVarKeys(regVStableSwap.contractId.bytes.arr)

        newState.contractInfo(makerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))

        newState.contractInfo(baseTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regBase.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(targetTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regTarget.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(maxOrderPerUserKey) shouldBe Some(DataEntry(Longs.toByteArray(5), DataType.Amount))
        newState.contractInfo(unitPriceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))
        newState.contractInfo(unitPriceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))

        val (baseTokenBalanceKey, targetTokenBalanceKey, userOrdersKey, orderOwnerKey, feeBaseKey, feeTargetKey, minBaseKey, maxBaseKey,
        minTargetKey, maxTargetKey, priceBaseKey, priceTargetKey, baseTokenLockedKey, targetTokenLockedKey, orderStatusKey) = getStableSwapContractStateMapKeys(regVStableSwap.contractId.bytes.arr, setOrder.id.arr, master)

        newState.contractNumInfo(baseTokenBalanceKey) shouldBe 1000L
        newState.contractNumInfo(targetTokenBalanceKey) shouldBe 1000L
        newState.contractNumInfo(userOrdersKey) shouldBe 0L
        newState.contractInfo(orderOwnerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(feeBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(feeTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(minBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(minTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(priceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(priceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractNumInfo(baseTokenLockedKey) shouldBe 0L
        newState.contractNumInfo(targetTokenLockedKey) shouldBe 0L
        newState.contractInfo(orderStatusKey) shouldBe Some(DataEntry(Array(0.toByte), DataType.Boolean))
      }
    }
  }

  val preconditionsAndVStableSwapSetAndSwapBaseToTarget: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unitPriceBase
      1) // unitPriceTarget
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    swapBaseToTarget <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 100, 0, 1, ts + 10, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, swapBaseToTarget)

  property("V Stable Swap able to swap base to target") {
    forAll(preconditionsAndVStableSwapSetAndSwapBaseToTarget) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, swapBaseToTarget: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTarget.timestamp, Seq(swapBaseToTarget), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (makerKey, baseTokenIdKey, targetTokenIdKey, maxOrderPerUserKey, unitPriceBaseKey, unitPriceTargetKey) = getStableSwapContractStateVarKeys(regVStableSwap.contractId.bytes.arr)

        newState.contractInfo(makerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(baseTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regBase.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(targetTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regTarget.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(maxOrderPerUserKey) shouldBe Some(DataEntry(Longs.toByteArray(5), DataType.Amount))
        newState.contractInfo(unitPriceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))
        newState.contractInfo(unitPriceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))

        val (baseTokenBalanceKey, targetTokenBalanceKey, userOrdersKey, orderOwnerKey, feeBaseKey, feeTargetKey, minBaseKey, maxBaseKey,
        minTargetKey, maxTargetKey, priceBaseKey, priceTargetKey, baseTokenLockedKey, targetTokenLockedKey, orderStatusKey) = getStableSwapContractStateMapKeys(regVStableSwap.contractId.bytes.arr, setOrder.id.arr, master)

        newState.contractNumInfo(baseTokenBalanceKey) shouldBe 400L
        newState.contractNumInfo(targetTokenBalanceKey) shouldBe 600L
        newState.contractNumInfo(userOrdersKey) shouldBe 1L
        newState.contractInfo(orderOwnerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(feeBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(feeTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(minBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(minTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(priceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(priceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractNumInfo(baseTokenLockedKey) shouldBe 600L
        newState.contractNumInfo(targetTokenLockedKey) shouldBe 400L
        newState.contractInfo(orderStatusKey) shouldBe Some(DataEntry(Array(1.toByte), DataType.Boolean))
      }
    }
  }

  val preconditionsAndVStableSwapSetAndSwapTargetToBase: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000, // totalSupplyBase
      1, // unityBase
      1000, // issueAmountBase
      1000, // totalSupplyTarget
      1, // unityTarget
      1000, // issueAmountTarget
      5, // maxOrderPerUser
      1, // unityPriceBase
      1) // unitPriceTarget
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    swapTargetToBase <- swapTargetToBaseVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 100, 0, 1, ts + 10, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, swapTargetToBase)

  property("V Stable Swap able to swap target to base") {
    forAll(preconditionsAndVStableSwapSetAndSwapTargetToBase) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, swapTargetToBase: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(swapTargetToBase.timestamp, Seq(swapTargetToBase), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = regBase.proofs.firstCurveProof.explicitGet().publicKey

        val (makerKey, baseTokenIdKey, targetTokenIdKey, maxOrderPerUserKey, unitPriceBaseKey, unitPriceTargetKey) = getStableSwapContractStateVarKeys(regVStableSwap.contractId.bytes.arr)

        newState.contractInfo(makerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(baseTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regBase.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(targetTokenIdKey) shouldBe Some(DataEntry(tokenIdFromBytes(regTarget.contractId.bytes.arr, Ints.toByteArray(0)).right.get.arr, DataType.TokenId))
        newState.contractInfo(maxOrderPerUserKey) shouldBe Some(DataEntry(Longs.toByteArray(5), DataType.Amount))
        newState.contractInfo(unitPriceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))
        newState.contractInfo(unitPriceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1), DataType.Amount))

        val (baseTokenBalanceKey, targetTokenBalanceKey, userOrdersKey, orderOwnerKey, feeBaseKey, feeTargetKey, minBaseKey, maxBaseKey,
        minTargetKey, maxTargetKey, priceBaseKey, priceTargetKey, baseTokenLockedKey, targetTokenLockedKey, orderStatusKey) = getStableSwapContractStateMapKeys(regVStableSwap.contractId.bytes.arr, setOrder.id.arr, master)

        newState.contractNumInfo(baseTokenBalanceKey) shouldBe 600L
        newState.contractNumInfo(targetTokenBalanceKey) shouldBe 400L
        newState.contractNumInfo(userOrdersKey) shouldBe 1L
        newState.contractInfo(orderOwnerKey) shouldBe Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(feeBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(feeTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(minBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(minTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(0L), DataType.Amount))
        newState.contractInfo(maxTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(100L), DataType.Amount))
        newState.contractInfo(priceBaseKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractInfo(priceTargetKey) shouldBe Some(DataEntry(Longs.toByteArray(1L), DataType.Amount))
        newState.contractNumInfo(baseTokenLockedKey) shouldBe 400L
        newState.contractNumInfo(targetTokenLockedKey) shouldBe 600L
        newState.contractInfo(orderStatusKey) shouldBe Some(DataEntry(Array(1.toByte), DataType.Boolean))
      }
    }
  }
}