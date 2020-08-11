package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.StateWriterImpl
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidTokenIndex}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.Left

class TDBAROpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  val state: StateWriterImpl = newState()

  val tx: RegisterContractTransaction = RegisterContractTransaction.create(
    PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
    ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
    "vsys", 10000L, 100, 1L).right.get

  val executionContext: ExecutionContext = ExecutionContext.fromRegConTx(
    state, TestFunctionalitySettings.Enabled, Option(0L),
    1L, 1, tx).right.get

  val account: Array[Byte] = PrivateKeyAccount(
    Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr

  property("test TDBAR opcs") {

    TDBAROpcDiff.balance(executionContext)(DataEntry(
      account, DataType.Address),
      DataEntry(Ints.toByteArray(0), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidTokenIndex))
    TDBAROpcDiff.balance(executionContext)(DataEntry(
      account, DataType.Address),
      DataEntry(Longs.toByteArray(0), DataType.Amount), Seq.empty, 0) should be (
      Left(ContractDataTypeMismatch))

    TDBAROpcDiff.balanceWithoutTokenIndex(
      executionContext)(DataEntry(
      account, DataType.Address),
      Seq.empty, 0) should be (
      Left(ContractInvalidTokenIndex))
  }
}
