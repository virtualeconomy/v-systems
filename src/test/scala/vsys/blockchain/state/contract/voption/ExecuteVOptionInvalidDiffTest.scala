package test.scala.vsys.blockchain.state.contract.voption

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.contract.voption.{VOptionContractGen, VOptionFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract._
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

  val preconditionsAndVOptionDepositAndWithdrawBaseTargetTokens: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (genesis, genesis2, master, _, regBaseTokenContract, _, _, _, regVOptionContract,
    issueBaseToken, _, _, _, depositBaseToken, _, _, _, fee, ts, attach) <- createBaseTargetOptionProofTokenAndInitVOption(1000, 1, 1000, 1000, 1, 100,
      100, 100,100, 100, 100, 10)
    withdrawBaseToken <- withdrawToken(master, regBaseTokenContract.contractId, regVOptionContract.contractId.bytes.arr, master.toAddress.bytes.arr, 100L, fee, ts + 13)
    withdrawInvalidBaseToken <- withdrawToken(master, regBaseTokenContract.contractId, regVOptionContract.contractId.bytes.arr, master.toAddress.bytes.arr, 2000L, fee, ts + 13)
  } yield (genesis, genesis2, regBaseTokenContract, regVOptionContract, issueBaseToken, depositBaseToken, withdrawBaseToken, withdrawInvalidBaseToken)

  // withdraw base tokens
  property("withdraw base tokens more than depositing in voption contract") {
    forAll(preconditionsAndVOptionDepositAndWithdrawBaseTargetTokens) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, regBaseTokenContract: RegisterContractTransaction, regVOptionContract: RegisterContractTransaction, issueBaseToken: ExecuteContractFunctionTransaction,
    depositBaseToken: ExecuteContractFunctionTransaction, withdrawBaseToken: ExecuteContractFunctionTransaction, withdrawInvalidBaseToken: ExecuteContractFunctionTransaction) =>
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
}
