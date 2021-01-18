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

  val preconditionsAndVOptionActivate: Gen[(GenesisTransaction, GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, genesis2, master, user, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000, 1, 1000, 1000, 1, 1000,
      1000, 1,1000, 1, 1000, 1000, 1000, 1000)
    activate <- activateVOptionGen(master, regVOptionContract.contractId, 1000, 1,1, attach, fee + 10000000000L, ts+13)
    activateInvalid <- activateVOptionGen(master, regVOptionContract.contractId, 10000, 1,1, attach, fee + 10000000000L, ts+13)
    activateInvalid2 <- activateVOptionGen(user, regVOptionContract.contractId, 1000, 1,1, attach, fee + 10000000000L, ts+13)
    activateInvalid3 <- activateVOptionGen(master, regVOptionContract.contractId, 1000, 1,0, attach, fee + 10000000000L, ts+13)
    activateInvalid4 <- activateVOptionGen(master, regVOptionContract.contractId, 1000, 0,1, attach, fee + 10000000000L, ts+13)
    activateInvalid5 <- activateVOptionGen(master, regVOptionContract.contractId, 0, 1,1, attach, fee + 10000000000L, ts+13)
  } yield (genesis, genesis2, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, activateInvalid, activateInvalid2, activateInvalid3, activateInvalid4, activateInvalid5)

  property("unable to activate voption contract") {
    forAll(preconditionsAndVOptionActivate) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC,
    regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC,
    issueOptionToken: EC, issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC, depositOptionToken: EC, depositProofToken: EC, activate: EC, activateInvalid: EC, activateInvalid2: EC, activateInvalid3: EC, activateInvalid4: EC, activateInvalid5: EC) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activate.timestamp, Seq(activate), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // activate amount more than depositing in voption contract
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid.timestamp, Seq(activateInvalid), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // activated by another user
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid2.timestamp, Seq(activateInvalid2), TransactionStatus.ContractInvalidCaller)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }
      // activated with price unit zero
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid3.timestamp, Seq(activateInvalid3), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // activated with price zero
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid4.timestamp, Seq(activateInvalid4), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // activated with maxIssueNum zero
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid4.timestamp, Seq(activateInvalid5), TransactionStatus.Failed)) { (blockDiffEi) =>
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

  property("unable to mint voption") {
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

  val preconditionsAndVOptionUnlock: Gen[(GenesisTransaction, GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, genesis2, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000, 1, 1000, 1000, 1, 1000,
      100, 1,100, 1, 1000, 1000, 100, 100)

    activate <- activateVOptionGen(master, regVOptionContract.contractId, 100, 1,1, attach, fee + 10000000000L, ts+13)
    mint <- mintVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+14)
    unlock <- unlockVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+14)
    unlockInvalid <- unlockVOptionGen(master, regVOptionContract.contractId, 100, attach, fee + 10000000000L, ts+14)
    unlockInvalid2 <- unlockVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+201)
  } yield (genesis, genesis2, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, mint, unlock, unlockInvalid, unlockInvalid2)

  property("unable to unlock") {
    forAll(preconditionsAndVOptionUnlock) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC, regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC,
    issueOptionToken: EC, issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC, depositOptionToken: EC, depositProofToken: EC, activate: EC, mint: EC, unlock: EC,
    unlockInvalid: EC, unlockInvalid2: EC) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(unlock.timestamp, Seq(unlock), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // unlock voption greater than mint amount
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(unlockInvalid.timestamp, Seq(unlockInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // unlock voption after executedeadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(regVOptionContract.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(unlockInvalid2.timestamp, Seq(unlockInvalid2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVOptionExecute: Gen[(GenesisTransaction, GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, genesis2, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000L, 1L, 1000L, 1000L, 1L, 1000L,
      1000L, 1L,1000L, 1L, 1000L, 1000L, 1000, 1000)

    activate <- activateVOptionGen(master, regVOptionContract.contractId, 1000L, 10L,1L, attach, fee + 10000000000L, ts+13)
    mint <- mintVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+14)

    execute <- executeVOptionGen(master, regVOptionContract.contractId, 10L, attach, fee + 10000000000L, ts+101)
    executeInvalid <- executeVOptionGen(master, regVOptionContract.contractId, 1000L, attach, fee + 10000000000L, ts+101)
    executeInvalid2 <- executeVOptionGen(master, regVOptionContract.contractId, 1000L, attach, fee + 10000000000L, ts+99)
    executeInvalid3 <- executeVOptionGen(master, regVOptionContract.contractId, 1000L, attach, fee + 10000000000L, ts+201)
  } yield (genesis, genesis2, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, mint, execute, executeInvalid, executeInvalid2, executeInvalid3)

  property("unable to execute voption") {
    forAll(preconditionsAndVOptionExecute) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC,
    regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC, issueOptionToken: EC, issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC,
    depositOptionToken: EC, depositProofToken: EC, activate: EC, mint: EC, execute: EC, executeInvalid: EC, executeInvalid2: EC, executeInvalid3: EC) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(execute.timestamp, Seq(execute), TransactionStatus.Success)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // execute voption more than target token balance
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(executeInvalid.timestamp, Seq(executeInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // execute voption before execute time
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(executeInvalid2.timestamp, Seq(executeInvalid2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // execute voption after execute deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(executeInvalid3.timestamp, Seq(executeInvalid3), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
  val preconditionsAndVOptionCollect: Gen[(GenesisTransaction, GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, genesis2, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000L, 1L, 1000L, 1000L, 1L, 1000L,
      1000L, 1L,1000L, 1L, 1000L, 1000L, 1000, 1000)

    activate <- activateVOptionGen(master, regVOptionContract.contractId, 1000L, 10L,1L, attach, fee + 10000000000L, ts+13)
    mint <- mintVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+14)
    execute <- executeVOptionGen(master, regVOptionContract.contractId, 2L, attach, fee + 10000000000L, ts+199)
    collect <- collectVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+100000000)
    collectInvalid <- collectVOptionGen(master, regVOptionContract.contractId, 1000L, attach, fee + 10000000000L, ts+100000000)
    collectInvalid2 <- collectVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+99)
    collectInvalid3 <- collectVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+199)

  } yield (genesis, genesis2, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, mint, execute, collect, collectInvalid, collectInvalid2, collectInvalid3)

  property("unable to collect voption") {
    forAll(preconditionsAndVOptionCollect) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC,
    regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC,
    issueOptionToken: EC, issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC, depositOptionToken: EC, depositProofToken: EC,
    activate: EC, mint: EC, execute: EC, collect: EC, collectInvalid: EC, collectInvalid2: EC, collectInvalid3: EC) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(activate.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(execute.timestamp, Seq()),
        TestBlock.create(execute.timestamp+1, Seq(execute))),
        TestBlock.createWithTxStatus(collect.timestamp, Seq(collect), TransactionStatus.Success)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // collect voption more than mint amount
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(execute.timestamp, Seq()),
        TestBlock.create(execute.timestamp+1, Seq(execute))),
        TestBlock.createWithTxStatus(collectInvalid.timestamp, Seq(collectInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // collect voption before execute time
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(execute.timestamp, Seq()),
        TestBlock.create(execute.timestamp+1, Seq(execute))),
        TestBlock.createWithTxStatus(collectInvalid2.timestamp, Seq(collectInvalid2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // collect voption after execute deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(execute.timestamp, Seq()),
        TestBlock.create(execute.timestamp+1, Seq(execute))),
        TestBlock.createWithTxStatus(collectInvalid3.timestamp, Seq(collectInvalid3), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}