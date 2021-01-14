
package vsys.blockchain.state.contract.voption

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.token.SystemContractGen
import vsys.blockchain.contract.voption.{VOptionContractGen, VOptionFunctionHelperGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract.{RegisterContractTransaction => RC, ExecuteContractFunctionTransaction => EC}
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}

class ExecuteVOptionValidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with VOptionContractGen
  with VOptionFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVOptionDepositBaseToken: Gen[(GenesisTransaction, RC,
    RC, RC, RC, RC, EC, EC, EC, EC)] = for {
    // Generates 4 register token contract transactions and a register contract tx for V Option
    // Also generates 4 deposit functions, proof and option token deposits the entire supply, deposit amount for base and target tokens can be selected
    (genesis, genesis2, master, user, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken , depositProofToken, fee, ts, attach) <-
      createBaseTargetOptionProofTokenAndInitVOption(
        1000L, // baseTotalSupply
        1L, // baseUnity
        1000L, // baseIssueAmount
        1000L, // targetTotalSupply
        1L, // targetUnity
        1000L, // targetIssueAmount
        1000L, // optionTotalSupply
        1L, // optionUnity
        1000L, // proofTotalSupply
        1L, // proofUnity
        100L, // baseTokenDepositAmount
        500L, // targetTokenDepositAmount
        1000L, // optionTokenDepositAmount
        1000L) // targetTokenDepositAmount

  } yield (genesis, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken,
    issueTargetToken, depositBaseToken, depositTargetToken)

  property("vOption able to deposit") {
    forAll(preconditionsAndVOptionDepositBaseToken) { case (genesis: GenesisTransaction, registerBase: RC,
    registerTarget: RC, registerOption: RC, registerProof: RC,
    registerVOption: RC, issueBase: EC, issueTarget: EC,
    depositBase: EC, depositTarget: EC) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)),
        TestBlock.create(registerVOption.timestamp, Seq(registerBase, registerTarget, registerOption, registerProof, registerVOption, issueBase, issueTarget,
          depositBase))),
        TestBlock.createWithTxStatus(depositTarget.timestamp, Seq(depositTarget), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = registerVOption.proofs.firstCurveProof.explicitGet().publicKey

        val (contractBaseTokenBalanceKey, contractTargetTokenBalanceKey, _, _) = getOptionContractTokenBalanceKeys(registerBase.contractId.bytes.arr,
          registerTarget.contractId.bytes.arr, registerOption.contractId.bytes.arr,
          registerProof.contractId.bytes.arr, registerVOption.contractId.bytes.arr)

        val (masterBaseTokenBalanceKey, masterTargetTokenBalanceKey, _, _) = getOptionUserTokenBalanceKeys(registerBase.contractId.bytes.arr,
          registerTarget.contractId.bytes.arr, registerOption.contractId.bytes.arr,
          registerProof.contractId.bytes.arr, master)

        newState.tokenAccountBalance(masterBaseTokenBalanceKey) shouldBe 900L // issue 1000, deposit 100 into contract
        newState.tokenAccountBalance(contractBaseTokenBalanceKey) shouldBe 100L // deposit 100 into contract

        newState.tokenAccountBalance(masterTargetTokenBalanceKey) shouldBe 500L // issue 1000, deposit 500 into contract
        newState.tokenAccountBalance(contractTargetTokenBalanceKey) shouldBe 500L // deposit 500 into contract
      }
    }
  }
}
