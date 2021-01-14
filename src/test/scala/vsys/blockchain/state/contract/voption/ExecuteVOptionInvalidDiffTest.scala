package test.scala.vsys.blockchain.state.contract.voption

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.contract.voption.{VOptionContractGen, VOptionFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction => EC, RegisterContractTransaction => RC}
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.state._

class ExecuteVOptionInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with VOptionContractGen
  with VOptionFunctionHelperGen {
  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVOptionDepositAndWithdrawBaseTargetTokens: Gen[(GenesisTransaction, GenesisTransaction, RC, RC, EC, EC, EC, EC)] = for {
    (genesis, genesis2, master, _, regBaseTokenContract, _, _, _, regVOptionContract,
    issueBaseToken, _, _, _, depositBaseToken, _, _, _, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000L, 1L, 1000L, 1000L, 1L, 1000L,
      100L, 1L, 100L, 1L, 100L, 100L, 1000L, 1000L)
    withdrawBaseToken <- withdrawToken(master, regBaseTokenContract.contractId, regVOptionContract.contractId.bytes.arr, master.toAddress.bytes.arr, 100L, fee, ts + 13)
    withdrawInvalidBaseToken <- withdrawToken(master, regBaseTokenContract.contractId, regVOptionContract.contractId.bytes.arr, master.toAddress.bytes.arr, 2000L, fee, ts + 13)
  } yield (genesis, genesis2, regBaseTokenContract, regVOptionContract, issueBaseToken, depositBaseToken, withdrawBaseToken, withdrawInvalidBaseToken)

  // withdraw base tokens
  property("withdraw base tokens more than depositing in voption contract") {
    forAll(preconditionsAndVOptionDepositAndWithdrawBaseTargetTokens) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regBaseTokenContract: RC, regVOptionContract: RC, issueBaseToken: EC,
    depositBaseToken: EC, withdrawBaseToken: EC, withdrawInvalidBaseToken: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regVOptionContract, issueBaseToken, depositBaseToken))),
        TestBlock.createWithTxStatus(withdrawBaseToken.timestamp, Seq(withdrawBaseToken), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositBaseToken.timestamp, Seq(regBaseTokenContract, regVOptionContract, issueBaseToken, depositBaseToken))),
        TestBlock.createWithTxStatus(withdrawInvalidBaseToken.timestamp, Seq(withdrawInvalidBaseToken), TransactionStatus.ContractTokenBalanceInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  val preconditionsAndVOptionActivate: Gen[(GenesisTransaction, GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, genesis2, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000, 1, 1000, 1000, 1, 1000,
      1000, 1,1000, 1, 1000, 1000, 1000, 1000)
    activate <- activateVOptionGen(master, regVOptionContract.contractId, 1000, 1,1, attach, fee + 10000000000L, ts+13)
    activateInvalid <- activateVOptionGen(master, regVOptionContract.contractId, 10000, 1,1, attach, fee + 10000000000L, ts+13)

  } yield (genesis, genesis2, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, activateInvalid)

  property("activate voption more than depositing in voption contract") {
    forAll(preconditionsAndVOptionActivate) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC,
    regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC,
    issueOptionToken: EC, issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC, depositOptionToken: EC, depositProofToken: EC, activate: EC, activateInvalid: EC) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activate.timestamp, Seq(activate), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid.timestamp, Seq(activateInvalid), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVOptionMint: Gen[(GenesisTransaction, GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, genesis2, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000, 1, 1000, 1000, 1, 1000,
      100, 1,100, 1, 1000, 1000, 100, 100)

    activate <- activateVOptionGen(master, regVOptionContract.contractId, 100, 1,1, attach, fee + 10000000000L, ts+13)
    mint <- mintVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+14)
    mintInvalid <- mintVOptionGen(master, regVOptionContract.contractId, 1000, attach, fee + 10000000000L, ts+14)
    mintInvalid2 <- mintVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+101)
    mintInvalid3 <- mintVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+201)
  } yield (genesis, genesis2, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, mint, mintInvalid, mintInvalid2, mintInvalid3)

  property("unable to mint") {
    forAll(preconditionsAndVOptionMint) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC,
    regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC, issueOptionToken: EC,
    issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC, depositOptionToken: EC, depositProofToken: EC,
    activate: EC, mint: EC, mintInvalid: EC, mintInvalid2: EC, mintInvalid3: EC) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(mint.timestamp, Seq(mint), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // mint voption is greater than maxIssueNum
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(mintInvalid.timestamp, Seq(mintInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // mint after executeTime
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(mintInvalid2.timestamp, Seq(mintInvalid2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // mint after executeDeadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(mintInvalid3.timestamp, Seq(mintInvalid3), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }




}