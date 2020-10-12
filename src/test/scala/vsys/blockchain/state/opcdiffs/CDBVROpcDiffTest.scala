package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.StateWriterImpl
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.ValidationError._
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.Left

class CDBVROpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  val state: StateWriterImpl = newState()

  val tx: RegisterContractTransaction = RegisterContractTransaction.create(
    PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
    ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
    "vsys", 10000L, 100, 1L).right.get

  val executionContext: ExecutionContext = ExecutionContext.fromRegConTx(
    state, TestFunctionalitySettings.Enabled, Option(0L),
    1L, 1, tx).right.get

  property("test CDBVR opcs") {
    CDBVROpcDiff.get(executionContext)(
      Array[Byte](0.toByte, 3.toByte), Seq.empty, 0) should be (Left(ContractStateVariableNotDefined))
    CDBVROpcDiff.get(executionContext)(
      Array[Byte](0.toByte, 3.toByte), Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    CDBVROpcDiff.mapGet(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), Seq.empty, 0) should be (
      Left(ContractStateMapNotDefined))
    CDBVROpcDiff.mapGet(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Ints.toByteArray(1), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidStateMap))
    CDBVROpcDiff.mapGet(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))

    CDBVROpcDiff.mapGetOrDefault(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), Seq.empty, 0) should be (
      Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Amount))))
    CDBVROpcDiff.mapGetOrDefault(executionContext)(
      Array[Byte](0.toByte, 9.toByte, 9.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Timestamp), Seq.empty, 0) should be (
      Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Timestamp))))
    CDBVROpcDiff.mapGetOrDefault(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Ints.toByteArray(1), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractInvalidStateMap))
    CDBVROpcDiff.mapGetOrDefault(executionContext)(
      Array[Byte](0.toByte, 4.toByte, 4.toByte), DataEntry(
        Ints.toByteArray(1), DataType.Int32), Seq.empty, 0) should be (
      Left(ContractStateMapNotDefined))
    CDBVROpcDiff.mapGetOrDefault(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))

    CDBVROpcDiff.getOrDefault(executionContext)(
      Array[Byte](0.toByte, 3.toByte), Seq.empty, 0) should be (
      Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Amount))))
    CDBVROpcDiff.getOrDefault(executionContext)(
      Array[Byte](0.toByte, 9.toByte), Seq.empty, 0) should be (
      Right(Seq(DataEntry(Longs.toByteArray(0), DataType.Timestamp))))
    CDBVROpcDiff.getOrDefault(executionContext)(
      Array[Byte](0.toByte, 4.toByte), Seq.empty, 0) should be (
      Left(ContractStateVariableNotDefined))
    CDBVROpcDiff.getOrDefault(executionContext)(
      Array[Byte](0.toByte, 3.toByte), Seq.empty, 1) should be (
      Left(ContractLocalVariableIndexOutOfRange))
  }
}
