package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.{TransactionGen, TransactionParser}
import vsys.blockchain.transaction.ValidationError.{ContractInvalidStateMap, ContractLocalVariableIndexOutOfRange, ContractStateMapNotDefined, ContractStateVariableNotDefined}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.Left

class CDBVROpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  val state = newState()

  val tx = RegisterContractTransaction.create(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
    ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
    "vsys", 10000L, 100, 1L)

  property("test CDBVR opcs") {
    CDBVROpcDiff.get(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 3.toByte), Seq.empty, 0) should be (Left(ContractStateVariableNotDefined))
    CDBVROpcDiff.get(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 3.toByte), Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    CDBVROpcDiff.mapGet(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), Seq.empty, 0) should be (
      Left(ContractStateMapNotDefined))
    CDBVROpcDiff.mapGet(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Ints.toByteArray(1), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidStateMap))
    CDBVROpcDiff.mapGet(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))

    CDBVROpcDiff.mapGetOrDefault(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), Seq.empty, 0) should be (
      Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Amount))))
    CDBVROpcDiff.mapGetOrDefault(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 9.toByte, 9.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Timestamp), Seq.empty, 0) should be (
      Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Timestamp))))
    CDBVROpcDiff.mapGetOrDefault(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Ints.toByteArray(1), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidStateMap))
    CDBVROpcDiff.mapGetOrDefault(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 4.toByte, 4.toByte), DataEntry(
        Ints.toByteArray(1), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractStateMapNotDefined))
    CDBVROpcDiff.mapGetOrDefault(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))
  }
}
