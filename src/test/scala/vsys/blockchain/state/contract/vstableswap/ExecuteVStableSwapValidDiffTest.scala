package vsys.blockchain.state.contract.vstableswap

import com.google.common.primitives.Bytes
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.SystemContractGen
import vsys.blockchain.contract.vstableswap.{VStableSwapContractGen, VStableSwapFunctionHelperGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
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
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget)

  property("V Stable Swap able to deposit") {
    forAll(preconditionsAndVStableSwapDepositToken) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase))),
        TestBlock.createWithTxStatus(depositBase.timestamp, Seq(depositBase), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueTarget))),
        TestBlock.createWithTxStatus(depositBase.timestamp, Seq(depositTarget), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVStableSwapWithdrawToken: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, _)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
    withdrawBase <- withdrawToken(master, regTokenBase.contractId, regVStableSwapContract.contractId.bytes.arr, master.toAddress.bytes.arr, 1000, fee, ts + 7)
    withdrawTarget <- withdrawToken(master, regTokenTarget.contractId, regVStableSwapContract.contractId.bytes.arr, master.toAddress.bytes.arr, 1000, fee, ts + 8)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, withdrawBase, withdrawTarget)

  property("V Stable Swap able to withdraw") {
    forAll(preconditionsAndVStableSwapWithdrawToken) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, withdrawBase: EC, withdrawTarget: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositBase.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, depositBase))),
        TestBlock.createWithTxStatus(withdrawBase.timestamp, Seq(withdrawBase), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueTarget, depositTarget))),
        TestBlock.createWithTxStatus(withdrawTarget.timestamp, Seq(withdrawTarget), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVStableSwapSupersedeAndSetOrder: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, user, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
    supersede <- supersedeVStableSwapGen(master, regVStableSwapContract.contractId, user.toAddress, attach, fee, ts + 5)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 1000, 1000, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, supersede, setOrder)

  property("V Stable Swap able to supersede and set order") {
    forAll(preconditionsAndVStableSwapSupersedeAndSetOrder) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, supersede: EC, setOrder: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositBase.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, supersede))),
        TestBlock.createWithTxStatus(setOrder.timestamp, Seq(setOrder), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVStableSwapSetAndUpdateOrder: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 1000, 1000, attach, fee, ts + 5)
    updateOrder <- updateVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10, 10, 10, 100, 10, 100, 5, 5, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, updateOrder)

  property("V Stable Swap able to set and update orders") {
    forAll(preconditionsAndVStableSwapSetAndUpdateOrder) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, updateOrder: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(updateOrder.timestamp, Seq(updateOrder), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val contractId = regVStableSwap.contractId.bytes

        val orderId = DataEntry.create(setOrder.id.arr, DataType.ShortBytes).right.get.bytes
        val orderOwnerKey = ByteStr(Bytes.concat(contractId.arr, Array(3.toByte), orderId))

        newState.contractInfo(orderOwnerKey) shouldBe Some(DataEntry(regVStableSwap.proofs.firstCurveProof.explicitGet().publicKey.toAddress.bytes.arr, DataType.Address))
      }
    }
  }

  val preconditionsAndVStableSwapSetAndOrderDeposit: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    orderDeposit <- orderDepositVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 500, 500, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, orderDeposit)

  property("V Stable Swap able to set and deposit to orders") {
    forAll(preconditionsAndVStableSwapSetAndOrderDeposit) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, orderDeposit: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderDeposit.timestamp, Seq(orderDeposit), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVStableSwapSetAndOrderWithdraw: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    orderWithdraw <- orderWithdrawVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 500, 500, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, orderWithdraw)

  property("V Stable Swap able to set and withdraw from orders") {
    forAll(preconditionsAndVStableSwapSetAndOrderWithdraw) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, orderWithdraw: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderWithdraw.timestamp, Seq(orderWithdraw), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVStableSwapSetAndOrderClose: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    orderClose <- closeVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, orderClose)

  property("V Stable Swap able to close orders") {
    forAll(preconditionsAndVStableSwapSetAndOrderClose) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, orderClose: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderClose.timestamp, Seq(orderClose), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVStableSwapSetAndSwapBaseToTarget: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    swapBaseToTarget <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 100, 0, 1, ts + 10, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, swapBaseToTarget)

  property("V Stable Swap able to swap base to target") {
    forAll(preconditionsAndVStableSwapSetAndSwapBaseToTarget) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, swapBaseToTarget: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTarget.timestamp, Seq(swapBaseToTarget), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndVStableSwapSetAndSwapTargetToBase: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
      <- createBaseTokenTargetTokenAndInitVStableSwap(
      1000,
      1,
      1000,
      1000,
      1,
      1000,
      5,
      1,
      1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 0, 100, 0, 100, 1, 1, 500, 500, attach, fee, ts + 5)
    swapTargetToBase <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 100, 0, 1, ts + 10, attach, fee, ts + 6)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, swapTargetToBase)

  property("V Stable Swap able to swap target to base") {
    forAll(preconditionsAndVStableSwapSetAndSwapTargetToBase) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, swapTargetToBase: EC) =>
      assertDiffAndState(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(setOrder.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(swapTargetToBase.timestamp, Seq(swapTargetToBase), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }
}