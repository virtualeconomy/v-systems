package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.{TransactionGen, TransactionParser}
import vsys.blockchain.transaction.ValidationError.ContractUnsupportedOPC
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.Left

class ReturnOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("test return opcs") {

    val state = newState()

    val tx = RegisterContractTransaction.create(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
      ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
      "vsys", 10000L, 100, 1L)

    ReturnOpcDiff.value(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(Seq.empty, 0) should be (
      Left(ContractUnsupportedOPC))
  }
}
