package test.scala.vsys.blockchain.state.contract.vstableswap

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

  val preconditionsAndVStableSwapUpdateOrder: Gen[(GenesisTransaction, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attachment)
      <- createBaseTokenTargetTokenAndInitVStableSwap(1000, 1, 1000, 1000,
      1, 1000, 5, 1, 1)
    setOrder <- setOrderVStableSwapGen(master, regVStableSwapContract.contractId, 0, 0, 10, 1000, 10, 1000, 10, 10, 1000, 1000, attachment, fee, ts+7)
    updateOrder <- updateVStableSwapGen(master, regVStableSwapContract.contractId, setOrder.id.arr, 0, 0, 10, 1000, 10, 1000, 10, 10, attachment, fee, ts+8)
    updateOrderInvalid <- updateVStableSwapGen(master, regVStableSwapContract.contractId, Array[Byte](10), 0, 0, 10, 1000, 10, 1000, 10, 10, attachment, fee, ts+8)

  } yield (genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, setOrder, updateOrder, updateOrderInvalid)

  property("unable to update order") {
    forAll(preconditionsAndVStableSwapUpdateOrder) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, setOrder: EC, updateOrder: EC, updateOrderInvalid: EC) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(updateOrder.timestamp, Seq(updateOrder), TransactionStatus.Success)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(regVStableSwap.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, setOrder))),
        TestBlock.createWithTxStatus(updateOrderInvalid.timestamp, Seq(updateOrderInvalid), TransactionStatus.ContractStateMapNotDefined)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractStateMapNotDefined
      }
    }
  }
}
