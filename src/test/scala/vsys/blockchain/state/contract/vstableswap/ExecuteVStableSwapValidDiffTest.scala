package vsys.blockchain.state.contract.vstableswap

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
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
    (genesis, genesis2, master, user, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, fee, ts, attach)
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
  } yield(genesis, regTokenBase, regTokenTarget, regVStableSwapContract, issueTokenBase, issueTokenTarget, depositBase, depositTarget, supersede, setOrder)

  property("V Stable Swap able to supersede and set order") {
    forAll(preconditionsAndVStableSwapSupersedeAndSetOrder) { case (genesis: GenesisTransaction, regBase: RC, regTarget: RC,
    regVStableSwap: RC, issueBase: EC, issueTarget: EC, depositBase: EC, depositTarget: EC, supersede: EC, setOrder: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositBase.timestamp, Seq(regBase, regTarget, regVStableSwap, issueBase, issueTarget, depositBase, depositTarget, supersede))),
        TestBlock.createWithTxStatus(setOrder.timestamp, Seq(setOrder), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }
}
