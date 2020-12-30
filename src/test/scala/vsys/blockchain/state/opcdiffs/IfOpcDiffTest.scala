package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Longs, Shorts}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.StateWriterImpl
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.{Left, Right}

class IfOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  val state: StateWriterImpl = newState()

  val tx: RegisterContractTransaction = RegisterContractTransaction.create(
    PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
    ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
    "vsys", 10000L, 100, 1L).right.get

  val executionContext: ExecutionContext = ExecutionContext.fromRegConTx(
    state, TestFunctionalitySettings.Enabled, Option(0L),
    1L, 1, tx).right.get

  property("if opcs") {
    IfOpcDiff.executeOpcBlock(executionContext,
      DataEntry(Longs.toByteArray(1), DataType.Amount),
      Seq.empty) should be (Left(ContractDataTypeMismatch))
    IfOpcDiff.executeOpcBlock(executionContext,
      DataEntry(Shorts.toByteArray(0), DataType.OpcBlock),
      Seq.empty) should be (Right((OpcDiff(Map(),Map(),Map(),Map(),Map(),Map(),Map(),Map()),List())))
    IfOpcDiff.executeOpcBlock(executionContext,
      DataEntry(Array[Byte](0, 2, 0, 1), DataType.OpcBlock),
      Seq.empty) should be (Left(ContractInvalidOPCData))

    IfOpcDiff.runCondition(
      DataEntry(Array(1.toByte), DataType.Boolean)
    ) should be (Right(true))
    IfOpcDiff.runCondition(
      DataEntry(Array(0.toByte), DataType.Boolean)
    ) should be (Right(false))
    IfOpcDiff.runCondition(
      DataEntry(Longs.toByteArray(1), DataType.Amount)
    ) should be (Left(ContractDataTypeMismatch))

    IfOpcDiff.parseBytes(executionContext)(
      Array[Byte](1.toByte, 0.toByte, 1.toByte),
      Seq[DataEntry](DataEntry(Array(1.toByte), DataType.Boolean),
        DataEntry(Shorts.toByteArray(0), DataType.OpcBlock))) should be (
      Right((OpcDiff(Map(),Map(),Map(),Map(),Map(),Map(),Map(),Map()),
        List(DataEntry(Array(1.toByte), DataType.Boolean),
        DataEntry(Shorts.toByteArray(0), DataType.OpcBlock)))))
    IfOpcDiff.parseBytes(executionContext)(
      Array[Byte](1.toByte, 0.toByte, 1.toByte),
      Seq[DataEntry](DataEntry(Array(0.toByte), DataType.Boolean),
        DataEntry(Shorts.toByteArray(0), DataType.OpcBlock))) should be (
      Right((OpcDiff(Map(),Map(),Map(),Map(),Map(),Map(),Map(),Map()),
        List(DataEntry(Array(0.toByte), DataType.Boolean),
          DataEntry(Shorts.toByteArray(0), DataType.OpcBlock)))))
    IfOpcDiff.parseBytes(executionContext)(
      Array[Byte](2.toByte, 0.toByte, 1.toByte, 2.toByte),
      Seq[DataEntry](DataEntry(Array(1.toByte), DataType.Boolean),
        DataEntry(Shorts.toByteArray(0), DataType.OpcBlock),
        DataEntry(Shorts.toByteArray(1), DataType.OpcBlock))) should be (
      Right((OpcDiff(Map(),Map(),Map(),Map(),Map(),Map(),Map(),Map()),
        List(DataEntry(Array(1.toByte), DataType.Boolean),
          DataEntry(Shorts.toByteArray(0), DataType.OpcBlock),
          DataEntry(Shorts.toByteArray(1), DataType.OpcBlock)))))
    IfOpcDiff.parseBytes(executionContext)(
      Array[Byte](2.toByte, 0.toByte, 1.toByte, 2.toByte),
      Seq[DataEntry](DataEntry(Array(0.toByte), DataType.Boolean),
        DataEntry(Shorts.toByteArray(0), DataType.OpcBlock),
        DataEntry(Array[Byte](0, 2, 0, 1), DataType.OpcBlock))) should be (
      Left(ContractInvalidOPCData))
  }
}
