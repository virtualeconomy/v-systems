package vsys.blockchain.state.contract.vstableswap

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.token.SystemContractGen
import vsys.blockchain.contract.vstableswap.{VStableSwapContractGen, VStableSwapFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction => EC, RegisterContractTransaction => RC}
import vsys.blockchain.state._

class ExecuteVStableSwapInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with VStableSwapContractGen
  with VStableSwapFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVStableSwapDepositToken: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, _, depositBase, _, fee, ts, _)
      <- createBaseTokenTargetTokenAndInitVStableSwap(1000, 1, 1000, 1000,
      1, 1000, 5, 1, 1)
    withdrawBase <- withdrawToken(master, regTokenBase.contractId, regVStableSwapContract.contractId.bytes.arr, master.toAddress.bytes.arr, 100, fee, ts+7)
    withdrawBaseInvalid <- withdrawToken(master, regTokenBase.contractId, regVStableSwapContract.contractId.bytes.arr, master.toAddress.bytes.arr, 10000, fee, ts+7)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, depositBase, withdrawBase, withdrawBaseInvalid)

  property("unable to withdraw tokens") {
    forAll(preconditionsAndVStableSwapDepositToken) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, depositBase: EC, withdrawBase: EC, withdrawBaseInvalid: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, depositBase))),
        TestBlock.createWithTxStatus(withdrawBase.timestamp, Seq(withdrawBase), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // withdraw tokens more than depositing in vstable swap contract
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, depositBase))),
        TestBlock.createWithTxStatus(withdrawBaseInvalid.timestamp, Seq(withdrawBaseInvalid), TransactionStatus.ContractTokenBalanceInsufficient)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  val preconditionsAndVStableSwapSetOrder: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attachment)
      <- createBaseTokenTargetTokenAndInitVStableSwap(1000, 1, 1000, 1000,
      1, 1000, 5, 1, 1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 10, 1000, 1000, attachment, fee, ts+7)
    setOrderInvalid <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 10, 10000, 1000, attachment, fee, ts+7)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, setOrderInvalid)

  property("unable to set order") {
    forAll(preconditionsAndVStableSwapSetOrder) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, setOrderInvalid: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget))),
        TestBlock.createWithTxStatus(setOrder.timestamp, Seq(setOrder), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // deposit into contract is less than deposit into order
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget))),
        TestBlock.createWithTxStatus(setOrderInvalid.timestamp, Seq(setOrderInvalid), TransactionStatus.ContractMapValueInsufficient)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  val preconditionsAndVStableSwapUpdateOrder: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attachment)
      <- createBaseTokenTargetTokenAndInitVStableSwap(1000, 1, 1000, 1000,
      1, 1000, 5, 1, 1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 10, 1000, 1000, attachment, fee, ts+7)
    closeOrder <- closeVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, attachment, fee, ts+8)
    updateOrder <- updateVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 0, 0, 10, 1000, 10, 1000, 10, 10, attachment, fee, ts+9)
    updateOrderInvalid <- updateVStableSwapGen(master, regVStableSwapContract.contractId, Array[Byte](10), 0, 0, 10, 1000, 10, 1000, 10, 10, attachment, fee, ts+9)

  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, closeOrder, updateOrder, updateOrderInvalid)

  property("unable to update order") {
    forAll(preconditionsAndVStableSwapUpdateOrder) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, closeOrder: EC, updateOrder: EC, updateOrderInvalid: EC) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(updateOrder.timestamp, Seq(updateOrder), TransactionStatus.Success)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // update order with wrong order id
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(updateOrderInvalid.timestamp, Seq(updateOrderInvalid), TransactionStatus.ContractStateMapNotDefined)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractStateMapNotDefined
      }
      // update order after close order
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder, closeOrder))),
        TestBlock.createWithTxStatus(updateOrder.timestamp, Seq(updateOrder), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVStableSwapOrderDeposit: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attachment)
      <- createBaseTokenTargetTokenAndInitVStableSwap(1000, 1, 1000, 1000,
      1, 1000, 5, 1, 1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 10, 990, 990, attachment, fee, ts+7)
    closeOrder <- closeVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, attachment, fee, ts+8)
    orderDeposit <- orderDepositVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10, 10, attachment, fee, ts+9)
    orderDepositInvalid <- orderDepositVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10000, 10, attachment, fee, ts+9)
    orderDepositInvalid2 <- orderDepositVStableSwapGen(master, regVStableSwapContract.contractId, Array[Byte](10), 10, 10, attachment, fee, ts+9)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, closeOrder, orderDeposit, orderDepositInvalid, orderDepositInvalid2)

  property("unable to order deposit") {
    forAll(preconditionsAndVStableSwapOrderDeposit) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, closeOrder: EC, orderDeposit: EC, orderDepositInvalid: EC, orderDepositInvalid2: EC) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderDeposit.timestamp, Seq(orderDeposit), TransactionStatus.Success)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // deposit into order is greater than deposit into contract
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderDepositInvalid.timestamp, Seq(orderDepositInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // order deposit with wrong order id
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderDepositInvalid2.timestamp, Seq(orderDepositInvalid2), TransactionStatus.ContractStateMapNotDefined)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractStateMapNotDefined
      }
      // order deposit after order close
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder, closeOrder))),
        TestBlock.createWithTxStatus(orderDeposit.timestamp, Seq(orderDeposit), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVStableSwapOrderWithdraw: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attachment)
      <- createBaseTokenTargetTokenAndInitVStableSwap(1000, 1, 1000, 1000,
      1, 1000, 5, 1, 1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 10, 990, 990, attachment, fee, ts+7)
    closeOrder <- closeVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, attachment, fee, ts+8)
    orderWithdraw <- orderWithdrawVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10, 10, attachment, fee, ts+9)
    orderWithdrawInvalid <- orderWithdrawVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10000, 10, attachment, fee, ts+9)
    orderWithdrawInvalid2 <- orderWithdrawVStableSwapGen(master, regVStableSwapContract.contractId, Array[Byte](10), 10, 10, attachment, fee, ts+9)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, closeOrder, orderWithdraw, orderWithdrawInvalid, orderWithdrawInvalid2)

  property("unable to order withdraw") {
    forAll(preconditionsAndVStableSwapOrderWithdraw) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, closeOrder: EC, orderWithdraw: EC, orderWithdrawInvalid: EC, orderWithdrawInvalid2: EC) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderWithdraw.timestamp, Seq(orderWithdraw), TransactionStatus.Success)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // withdraw from contract is greater than deposit into order
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderWithdrawInvalid.timestamp, Seq(orderWithdrawInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }

      // order withdraw with wrong order id
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(orderWithdrawInvalid2.timestamp, Seq(orderWithdrawInvalid2), TransactionStatus.ContractStateMapNotDefined)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractStateMapNotDefined
      }
      // order withdraw after order close
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder, closeOrder))),
        TestBlock.createWithTxStatus(orderWithdraw.timestamp, Seq(orderWithdraw), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVStableSwapOrderClose: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attachment)
      <- createBaseTokenTargetTokenAndInitVStableSwap(1000, 1, 1000, 1000,
      1, 1000, 5, 1, 1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 10, 1000, 1000, attachment, fee, ts+7)
    closeOrder <- closeVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, attachment, fee, ts+8)
    closeOrderInvalid <- closeVStableSwapGen(master, regVStableSwapContract.contractId, Array[Byte](10), attachment, fee, ts+8)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, closeOrder, closeOrderInvalid)

  property("unable to close order") {
    forAll(preconditionsAndVStableSwapOrderClose) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, closeOrder: EC, closeOrderInvalid: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(closeOrder.timestamp, Seq(closeOrder), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // close order with wrong order id
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(closeOrderInvalid.timestamp, Seq(closeOrderInvalid), TransactionStatus.ContractStateMapNotDefined)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractStateMapNotDefined
      }
    }
  }
  val preconditionsAndVStableSwapBaseToTarget: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attachment)
      <- createBaseTokenTargetTokenAndInitVStableSwap(Long.MaxValue, 1, Long.MaxValue, Long.MaxValue,
      1, Long.MaxValue, 5, 1, 1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 1, 990, 100000, attachment, fee, ts+7)
    setOrder1 <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 1, 990, 10, attachment, fee, ts+7)
    setOrder2 <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, Long.MaxValue, 10, 1000, 1, 1, 990, Long.MaxValue, attachment, fee, ts+7)
    closeOrder <- closeVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, attachment, fee, ts+8)
    swapBaseToTarget <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10, 0, 10, ts+100, attachment, fee, ts+9)
    swapBaseToTargetInvalid <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10, 0, 20, ts+100, attachment, fee, ts+9)
    swapBaseToTargetInvalid2 <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10, 10, 10, ts+100, attachment, fee, ts+9)
    swapBaseToTargetInvalid3 <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder1.id.arr, 10, 0, 10, ts+100, attachment, fee, ts+9)
    swapBaseToTargetInvalid4 <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder2.id.arr, Long.MaxValue, 0, 1, ts+100, attachment, fee, ts+9)
    swapBaseToTargetInvalid5 <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10, 0, 10, ts-100, attachment, fee, ts+9)
    swapBaseToTargetInvalid6 <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 10000, 0, 10, ts+100, attachment, fee, ts+9)
    swapBaseToTargetInvalid7 <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 1, 0, 10, ts+100, attachment, fee, ts+9)
    swapBaseToTargetInvalid8 <- swapBaseToTargetVStableSwapGen(master, regVStableSwapContract.contractId, Array[Byte](10), 10, 0, 10, ts+100, attachment, fee, ts+9)
  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, setOrder1, setOrder2, closeOrder, swapBaseToTarget, swapBaseToTargetInvalid, swapBaseToTargetInvalid2,
    swapBaseToTargetInvalid3, swapBaseToTargetInvalid4, swapBaseToTargetInvalid5, swapBaseToTargetInvalid6, swapBaseToTargetInvalid7, swapBaseToTargetInvalid8)

  property("unable to swap base to target") {
    forAll(preconditionsAndVStableSwapBaseToTarget) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, setOrder1: EC, setOrder2: EC, closeOrder: EC, swapBaseToTarget: EC, swapBaseToTargetInvalid: EC, swapBaseToTargetInvalid2: EC,
    swapBaseToTargetInvalid3: EC, swapBaseToTargetInvalid4: EC, swapBaseToTargetInvalid5: EC, swapBaseToTargetInvalid6: EC, swapBaseToTargetInvalid7: EC, swapBaseToTargetInvalid8: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTarget.timestamp, Seq(swapBaseToTarget), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // price is not equal to priceBase
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTargetInvalid.timestamp, Seq(swapBaseToTargetInvalid), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // swapFee is not equal to baseFee
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTargetInvalid2.timestamp, Seq(swapBaseToTargetInvalid2), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // swap base token with not enough depositing target token
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder1))),
        TestBlock.createWithTxStatus(swapBaseToTargetInvalid3.timestamp, Seq(swapBaseToTargetInvalid3), TransactionStatus.ContractMapValueInsufficient)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // swap base token with not enough holding base token
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder2))),
        TestBlock.createWithTxStatus(swapBaseToTargetInvalid4.timestamp, Seq(swapBaseToTargetInvalid4), TransactionStatus.ContractMapValueInsufficient)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // swap base token with wrong deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget)),
        TestBlock.create(setOrder.timestamp, Seq(setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTargetInvalid5.timestamp, Seq(swapBaseToTargetInvalid5), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // swap base token more than maxBase
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget)),
        TestBlock.create(setOrder.timestamp, Seq(setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTargetInvalid6.timestamp, Seq(swapBaseToTargetInvalid6), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // swap base token less than minBase
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget)),
        TestBlock.create(setOrder.timestamp, Seq(setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTargetInvalid7.timestamp, Seq(swapBaseToTargetInvalid7), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // swap base token with wrong order id
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget)),
        TestBlock.create(setOrder.timestamp, Seq(setOrder))),
        TestBlock.createWithTxStatus(swapBaseToTargetInvalid8.timestamp, Seq(swapBaseToTargetInvalid8), TransactionStatus.ContractStateMapNotDefined)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractStateMapNotDefined
      }
      // swap base token after order close
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder, closeOrder))),
        TestBlock.createWithTxStatus(swapBaseToTarget.timestamp, Seq(swapBaseToTarget), TransactionStatus.Failed)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}
