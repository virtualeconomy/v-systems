package test.scala.vsys.blockchain.state.contract.voption

import com.google.common.primitives.Ints
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.ContractGenHelper.basicContractTestGen
import vsys.blockchain.contract.DataEntry
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

  val preconditionsAndVOptionDepositAndWithdrawBaseTargetTokens: Gen[(GenesisTransaction, RC, RC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regBaseTokenContract, _, _, _, regVOptionContract,
    issueBaseToken, _, _, _, depositBaseToken, _, _, _, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000L, 1L, 1000L, 1000L, 1L, 1000L,
      100L, 1L, 100L, 1L, 100L, 100L, 1000L, 1000L)
    withdrawBaseToken <- withdrawToken(master, regBaseTokenContract.contractId, regVOptionContract.contractId.bytes.arr, master.toAddress.bytes.arr, 100L, fee, ts + 13)
    withdrawInvalidBaseToken <- withdrawToken(master, regBaseTokenContract.contractId, regVOptionContract.contractId.bytes.arr, master.toAddress.bytes.arr, 2000L, fee, ts + 13)
  } yield (genesis, regBaseTokenContract, regVOptionContract, issueBaseToken, depositBaseToken, withdrawBaseToken, withdrawInvalidBaseToken)

  // withdraw base tokens
  property("withdraw base tokens more than depositing in voption contract") {
    forAll(preconditionsAndVOptionDepositAndWithdrawBaseTargetTokens) { case (genesis: GenesisTransaction, regBaseTokenContract: RC, regVOptionContract: RC, issueBaseToken: EC,
    depositBaseToken: EC, withdrawBaseToken: EC, withdrawInvalidBaseToken: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositBaseToken.timestamp, Seq(regBaseTokenContract, regVOptionContract, issueBaseToken, depositBaseToken))),
        TestBlock.createWithTxStatus(withdrawBaseToken.timestamp, Seq(withdrawBaseToken), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositBaseToken.timestamp, Seq(regBaseTokenContract, regVOptionContract, issueBaseToken, depositBaseToken))),
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

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activate.timestamp, Seq(activate), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // activate amount more than depositing in voption contract
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid.timestamp, Seq(activateInvalid), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // activated by another user
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid2.timestamp, Seq(activateInvalid2), TransactionStatus.ContractInvalidCaller)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }
      // activated with price unit zero
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid3.timestamp, Seq(activateInvalid3), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // activated with price zero
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid4.timestamp, Seq(activateInvalid4), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // activated with maxIssueNum zero
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(activateInvalid5.timestamp, Seq(activateInvalid5), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

    }
  }

  val preconditionsAndVOptionMint: Gen[(GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000, 1, 1000, 1000, 1, 1000,
      100, 1,100, 1, 1000, 1000, 100, 100)

    activate <- activateVOptionGen(master, regVOptionContract.contractId, 100, 1,1, attach, fee + 10000000000L, ts+13)
    mint <- mintVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+14)
    mintInvalid <- mintVOptionGen(master, regVOptionContract.contractId, 1000, attach, fee + 10000000000L, ts+14)
    mintInvalid2 <- mintVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+101)
    mintInvalid3 <- mintVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+201)
  } yield (genesis, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, mint, mintInvalid, mintInvalid2, mintInvalid3)

  property("unable to mint voption") {
    forAll(preconditionsAndVOptionMint) { case (genesis: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC,
    regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC, issueOptionToken: EC,
    issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC, depositOptionToken: EC, depositProofToken: EC,
    activate: EC, mint: EC, mintInvalid: EC, mintInvalid2: EC, mintInvalid3: EC) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(activate.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(mint.timestamp, Seq(mint), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      // mint before activate
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(mint.timestamp, Seq(mint), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // mint voption is greater than maxIssueNum
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(activate.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(mintInvalid.timestamp, Seq(mintInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // mint after executeTime
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(activate.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(mintInvalid2.timestamp, Seq(mintInvalid2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // mint after executeDeadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(activate.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(mintInvalid3.timestamp, Seq(mintInvalid3), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVOptionUnlock: Gen[(GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000, 1, 1000, 1000, 1, 1000,
      100, 1,100, 1, 1000, 1000, 100, 100)

    activate <- activateVOptionGen(master, regVOptionContract.contractId, 100, 1,1, attach, fee + 10000000000L, ts+13)
    mint <- mintVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+14)
    unlock <- unlockVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+14)
    unlockInvalid <- unlockVOptionGen(master, regVOptionContract.contractId, 100, attach, fee + 10000000000L, ts+14)
    unlockInvalid2 <- unlockVOptionGen(master, regVOptionContract.contractId, 10, attach, fee + 10000000000L, ts+201)
  } yield (genesis, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, mint, unlock, unlockInvalid, unlockInvalid2)

  property("unable to unlock") {
    forAll(preconditionsAndVOptionUnlock) { case (genesis: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC, regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC,
    issueOptionToken: EC, issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC, depositOptionToken: EC, depositProofToken: EC, activate: EC, mint: EC, unlock: EC,
    unlockInvalid: EC, unlockInvalid2: EC) =>

      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(unlock.timestamp, Seq(unlock), TransactionStatus.Success)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      // unlock before activate
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(unlock.timestamp, Seq(unlock), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // unlock before mint
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(activate.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(unlock.timestamp, Seq(unlock), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }

      // unlock voption greater than mint amount
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(unlockInvalid.timestamp, Seq(unlockInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // unlock voption after executedeadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(unlockInvalid2.timestamp, Seq(unlockInvalid2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVOptionExecute: Gen[(GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000L, 1L, 1000L, 1000L, 1L, 1000L,
      1000L, 1L,1000L, 1L, 1000L, 1000L, 1000, 1000)

    activate <- activateVOptionGen(master, regVOptionContract.contractId, 1000L, 10L,1L, attach, fee + 10000000000L, ts+13)
    mint <- mintVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+14)

    execute <- executeVOptionGen(master, regVOptionContract.contractId, 10L, attach, fee + 10000000000L, ts+101)
    executeInvalid <- executeVOptionGen(master, regVOptionContract.contractId, 1000L, attach, fee + 10000000000L, ts+101)
    executeInvalid2 <- executeVOptionGen(master, regVOptionContract.contractId, 1000L, attach, fee + 10000000000L, ts+99)
    executeInvalid3 <- executeVOptionGen(master, regVOptionContract.contractId, 1000L, attach, fee + 10000000000L, ts+201)
  } yield (genesis, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, activate, mint, execute, executeInvalid, executeInvalid2, executeInvalid3)

  property("unable to execute voption") {
    forAll(preconditionsAndVOptionExecute) { case (genesis: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC,
    regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC, issueOptionToken: EC, issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC,
    depositOptionToken: EC, depositProofToken: EC, activate: EC, mint: EC, execute: EC, executeInvalid: EC, executeInvalid2: EC, executeInvalid3: EC) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(execute.timestamp, Seq(execute), TransactionStatus.Success)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Success
      }

      // execute voption before activate
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken))),
        TestBlock.createWithTxStatus(execute.timestamp, Seq(execute), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // execute before mint
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(activate.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate))),
        TestBlock.createWithTxStatus(execute.timestamp, Seq(execute), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }

      // execute voption more than target token balance
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(executeInvalid.timestamp, Seq(executeInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // execute voption before execute time
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(executeInvalid2.timestamp, Seq(executeInvalid2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // execute voption after execute deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint))),
        TestBlock.createWithTxStatus(executeInvalid3.timestamp, Seq(executeInvalid3), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
  val preconditionsAndVOptionCollect: Gen[(GenesisTransaction, RC, RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {
    (genesis, _, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000L, 1L, 1000L, 1000L, 1L, 1000L,
      1000L, 1L,1000L, 1L, 1000L, 1000L, 1000, 1000)

    activate <- activateVOptionGen(master, regVOptionContract.contractId, 1000L, 10L,1L, attach, fee + 10000000000L, ts+13)
    mint <- mintVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+14)
    unlock <- unlockVOptionGen(master, regVOptionContract.contractId, 100, attach, fee + 10000000000L, ts+15)

    collect <- collectVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+100000000)
    collectInvalid <- collectVOptionGen(master, regVOptionContract.contractId, 1000L, attach, fee + 10000000000L, ts+100000000)
    collectInvalid2 <- collectVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+99)
    collectInvalid3 <- collectVOptionGen(master, regVOptionContract.contractId, 100L, attach, fee + 10000000000L, ts+199)
    collectInvalid4 <- collectVOptionGen(master, regVOptionContract.contractId, 0L, attach, fee + 10000000000L, ts+100000000)
  } yield (genesis, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken,
    activate, mint, unlock, collect, collectInvalid, collectInvalid2, collectInvalid3, collectInvalid4)

  property("unable to collect voption") {
    forAll(preconditionsAndVOptionCollect) { case (genesis: GenesisTransaction, regBaseTokenContract: RC,
    regTargetTokenContract: RC, regOptionTokenContract: RC, regProofTokenContract: RC,
    regVOptionContract: RC, issueBaseToken: EC, issueTargetToken: EC,
    issueOptionToken: EC, issueProofToken: EC, depositBaseToken: EC, depositTargetToken: EC, depositOptionToken: EC, depositProofToken: EC,
    activate: EC, mint: EC, unlock: EC, collect: EC, collectInvalid: EC, collectInvalid2: EC, collectInvalid3: EC, collectInvalid4: EC) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(collect.timestamp, Seq())),
        TestBlock.createWithTxStatus(collect.timestamp+1, Seq(collect), TransactionStatus.Success)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
      // collect voption before activate
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(depositProofToken.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken)),
        TestBlock.create(collect.timestamp, Seq())),
        TestBlock.createWithTxStatus(collect.timestamp+1, Seq(collect), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // collect voption before mint
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(activate.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate)),
        TestBlock.create(collect.timestamp, Seq())),
        TestBlock.createWithTxStatus(collect.timestamp+1, Seq(collect), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // collect voption after unlock
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(unlock.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint, unlock)),
        TestBlock.create(collect.timestamp, Seq())),
        TestBlock.createWithTxStatus(collect.timestamp+1, Seq(collect), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // collect voption more than mint amount
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(collectInvalid.timestamp, Seq())),
        TestBlock.createWithTxStatus(collectInvalid.timestamp+1, Seq(collectInvalid), TransactionStatus.ContractMapValueInsufficient)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
      // collect voption before execute time
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(collectInvalid2.timestamp, Seq())),
        TestBlock.createWithTxStatus(collectInvalid2.timestamp+1, Seq(collectInvalid2), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
      // collect voption after execute deadline
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(collectInvalid3.timestamp, Seq())),
        TestBlock.createWithTxStatus(collectInvalid3.timestamp+1, Seq(collectInvalid3), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }

      // collect zero amount after collecting all amount
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(mint.timestamp, Seq(regBaseTokenContract, regTargetTokenContract,
        regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken,
        depositOptionToken, depositProofToken, activate, mint)),
        TestBlock.create(collect.timestamp, Seq()),
        TestBlock.create(collect.timestamp, Seq(collect)),
        TestBlock.create(collectInvalid4.timestamp, Seq())),
        TestBlock.createWithTxStatus(collectInvalid4.timestamp+1, Seq(collectInvalid4), TransactionStatus.Failed)) { (blockDiffEi, _) =>
        blockDiffEi.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVOptionDepositSameInputTokens: Gen[(GenesisTransaction, RC, RC, EC, EC)] = for {
    (master, ts, fee) <- basicContractTestGen()
    genesis <- genesisVOptionGen(master, ts)
    user <- accountGen
    genesis2 <- genesisVOptionGen(user, ts)
    vOptionContract <- vOptionContractGen()
    // register base token
    regBaseTokenContract <- registerToken(master, 1000000000, 1000, "init", fee + 10000000000L, ts)
    baseTokenContractId = regBaseTokenContract.contractId
    baseTokenId = tokenIdFromBytes(baseTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

    // issue base token
    attach <- genBoundedString(2, EC.MaxDescriptionSize)
    issueBaseToken <- issueToken(master, baseTokenContractId, 1000000000, fee, ts + 5)

    // register VOption contract with same token Ids
    description <- validDescStringGen
    initVOptionDataStack: Seq[DataEntry] <- initVOptionDataStackGen(baseTokenId.arr, baseTokenId.arr, baseTokenId.arr, baseTokenId.arr, ts + 100, ts + 200)
    regVOptionContract <- registerVOptionGen(master, vOptionContract, initVOptionDataStack, description, fee + 10000000000L, ts + 4)
    vOptionContractId = regVOptionContract.contractId

    invalidDeposit <- depositToken(master, baseTokenContractId, master.toAddress.bytes.arr, vOptionContractId.bytes.arr, 1000000000, fee + 10000000000L, ts + 9)
  } yield (genesis, regBaseTokenContract, regVOptionContract, issueBaseToken, invalidDeposit)

  property("unable to deposit tokens when 4 input token ids are the same") {
    forAll(preconditionsAndVOptionDepositSameInputTokens) { case (genesis: GenesisTransaction, regBaseTokenContract: RC, regVOptionContract: RC, issueBaseToken: EC,
    invalidDeposit: EC) =>
      assertDiffEi(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(issueBaseToken.timestamp, Seq(regBaseTokenContract, regVOptionContract, issueBaseToken))),
        TestBlock.createWithTxStatus(invalidDeposit.timestamp, Seq(invalidDeposit), TransactionStatus.Failed)) { (blockDiffEi) =>
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}
