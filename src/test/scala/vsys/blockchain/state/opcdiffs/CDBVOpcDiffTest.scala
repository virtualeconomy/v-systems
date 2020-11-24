package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.Longs
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.{ByteStr, StateWriterImpl}
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidStateMap,
  ContractInvalidStateVariable, ContractMapValueInsufficient, InvalidDataEntry}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.Left

class CDBVOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  val state: StateWriterImpl = newState()

  val tx: RegisterContractTransaction = RegisterContractTransaction.create(
    PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
    ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
    "vsys", 10000L, 100, 1L).right.get

  val executionContext: ExecutionContext = ExecutionContext.fromRegConTx(
    state, TestFunctionalitySettings.Enabled, Option(0L),
    1L, 1, tx).right.get

  property("test CDBV opcs") {
    ByteStr(CDBVOpcDiff.set(executionContext)(
      Array[Byte](0.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(0), DataType.Amount)).right.get.contractDB(
      ByteStr(tx.contractId.bytes.arr ++ Array[Byte](0.toByte)))) shouldEqual ByteStr(DataEntry(
        Longs.toByteArray(0), DataType.Amount).bytes)
    CDBVOpcDiff.set(executionContext)(
      Array[Byte](0.toByte, 2.toByte), DataEntry(
        Longs.toByteArray(0), DataType.Amount)) should be (Left(ContractInvalidStateVariable))

    ByteStr(CDBVOpcDiff.mapSet(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(0), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount)).right.get.contractDB(
      ByteStr(tx.contractId.bytes.arr ++ Array[Byte](0.toByte) ++ DataEntry(
        Longs.toByteArray(0), DataType.Amount).bytes))) shouldEqual ByteStr(DataEntry(
      Longs.toByteArray(0), DataType.Amount).bytes)
    CDBVOpcDiff.mapSet(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 2.toByte), DataEntry(
        Longs.toByteArray(0), DataType.Amount), DataEntry(
        Longs.toByteArray(0), DataType.Amount)) should be (Left(ContractInvalidStateMap))

    CDBVOpcDiff.mapValueAdd(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(0), DataType.Amount), DataEntry(
        Longs.toByteArray(0), DataType.Amount)).right.get.contractNumDB(
      ByteStr(tx.contractId.bytes.arr ++ Array[Byte](0.toByte) ++ DataEntry(
        Longs.toByteArray(0), DataType.Amount).bytes)) shouldEqual 0
    CDBVOpcDiff.mapValueAdd(executionContext)(
      Array[Byte](0.toByte, 2.toByte, 2.toByte), DataEntry(
        Longs.toByteArray(0), DataType.ShortBytes), DataEntry(
        Longs.toByteArray(0), DataType.Amount)) should be (Left(ContractInvalidStateMap))
    CDBVOpcDiff.mapValueAdd(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(0), DataType.Amount), DataEntry(
        Longs.toByteArray(-1), DataType.Amount)) should be (Left(InvalidDataEntry))
    CDBVOpcDiff.mapValueAdd(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 4.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), DataEntry(
        Longs.toByteArray(1), DataType.Int32)) should be (Left(ContractDataTypeMismatch))

    CDBVOpcDiff.mapValueMinus(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(0), DataType.Amount), DataEntry(
        Longs.toByteArray(0), DataType.Amount)).right.get.contractNumDB(
      ByteStr(tx.contractId.bytes.arr ++ Array[Byte](0.toByte) ++ DataEntry(
        Longs.toByteArray(0), DataType.Amount).bytes)) shouldEqual 0
    CDBVOpcDiff.mapValueMinus(executionContext)(
      Array[Byte](0.toByte, 2.toByte, 2.toByte), DataEntry(
        Longs.toByteArray(0), DataType.ShortBytes), DataEntry(
        Longs.toByteArray(0), DataType.Amount)) should be (Left(ContractInvalidStateMap))
    CDBVOpcDiff.mapValueMinus(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(0), DataType.Amount), DataEntry(
        Longs.toByteArray(-1), DataType.Amount)) should be (Left(InvalidDataEntry))
    CDBVOpcDiff.mapValueMinus(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 3.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), DataEntry(
        Longs.toByteArray(1), DataType.Amount)) should be (Left(ContractMapValueInsufficient))
    CDBVOpcDiff.mapValueMinus(executionContext)(
      Array[Byte](0.toByte, 3.toByte, 4.toByte), DataEntry(
        Longs.toByteArray(1), DataType.Amount), DataEntry(
        Longs.toByteArray(1), DataType.Int32)) should be (Left(ContractDataTypeMismatch))
  }
}
