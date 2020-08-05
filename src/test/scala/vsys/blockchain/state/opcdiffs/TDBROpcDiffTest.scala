package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.{TransactionGen, TransactionParser}
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidTokenIndex,
  ContractLocalVariableIndexOutOfRange}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.Left

class TDBROpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("test TDBR opcs") {

    val state = newState()

    val tx = RegisterContractTransaction.create(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
      ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
      "vsys", 10000L, 100, 1L)

    TDBROpcDiff.max(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Longs.toByteArray(0), DataType.Amount), Seq.empty, 0) should be (
      Left(ContractDataTypeMismatch))
    TDBROpcDiff.max(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))
    TDBROpcDiff.max(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidTokenIndex))

    TDBROpcDiff.total(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Longs.toByteArray(0), DataType.Amount), Seq.empty, 0) should be (
      Left(ContractDataTypeMismatch))
    TDBROpcDiff.total(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))
    TDBROpcDiff.total(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidTokenIndex))

    TDBROpcDiff.unity(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Longs.toByteArray(0), DataType.Amount), Seq.empty, 0) should be (
      Left(ContractDataTypeMismatch))
    TDBROpcDiff.unity(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))
    TDBROpcDiff.unity(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidTokenIndex))

    TDBROpcDiff.desc(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Longs.toByteArray(0), DataType.Amount), Seq.empty, 0) should be (
      Left(ContractDataTypeMismatch))
    TDBROpcDiff.desc(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))
    TDBROpcDiff.desc(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidTokenIndex))
  }
}
