package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.{TransactionGen, TransactionParser}
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidCaller,
  ContractInvalidSigner, GenericError}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.{Left, Right}

class AssertOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  val state = newState()

  property("test assert opcs") {
    AssertOpcDiff.assertTrue(DataEntry(Array(1.toByte), DataType.Boolean)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.assertTrue(DataEntry(Array(0.toByte), DataType.Boolean)) should be (
      Left(GenericError(s"Invalid Assert (Boolean True): " +
        s"Value ${DataEntry(Array(0.toByte), DataType.Boolean).json} is False")))
    AssertOpcDiff.assertTrue(DataEntry(Array(1.toByte), DataType.ShortBytes)) should be (Left(ContractDataTypeMismatch))

    AssertOpcDiff.gtEq0(DataEntry(Longs.toByteArray(1), DataType.Amount)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.gtEq0(DataEntry(Longs.toByteArray(0), DataType.Amount)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.gtEq0(DataEntry(Longs.toByteArray(-1), DataType.Amount)) should be (
      Left(GenericError(s"Invalid Assert (gteq0): " +
        s"Value ${DataEntry(Longs.toByteArray(-1), DataType.Amount).json} is negative")))

    AssertOpcDiff.ltEq(DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.ltEq(DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.ShortBytes)) should be (Left(ContractDataTypeMismatch))
    AssertOpcDiff.ltEq(DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(0), DataType.Amount)) should be (
      Left(GenericError(s"Invalid Assert (lteq0): " +
        s"Value ${DataEntry(Longs.toByteArray(1), DataType.Amount).json} " +
        s"is larger than ${DataEntry(Longs.toByteArray(0), DataType.Amount).json}")))

    AssertOpcDiff.ltInt64(DataEntry(Longs.toByteArray(1), DataType.Amount)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.ltInt64(DataEntry(Longs.toByteArray(0), DataType.ShortBytes)) should be (
      Left(ContractDataTypeMismatch))

    AssertOpcDiff.gt0(DataEntry(Longs.toByteArray(1), DataType.Amount)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.gt0(DataEntry(Longs.toByteArray(0), DataType.Amount)) should be (
      Left(GenericError(s"Invalid Assert (gt0): " +
        s"Value ${DataEntry(Longs.toByteArray(0), DataType.Amount).json} is non-positive")))
    AssertOpcDiff.gt0(DataEntry(Longs.toByteArray(-1), DataType.Amount)) should be (
      Left(GenericError(s"Invalid Assert (gt0): " +
        s"Value ${DataEntry(Longs.toByteArray(-1), DataType.Amount).json} is non-positive")))

    AssertOpcDiff.equal(DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Longs.toByteArray(1), DataType.Amount)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.equal(DataEntry(Longs.toByteArray(1), DataType.Amount),
      DataEntry(Ints.toByteArray(1), DataType.Int32)) should be (
      Left(GenericError(s"Invalid Assert (eq): DataEntry " +
        s"${DataEntry(Longs.toByteArray(1), DataType.Amount).json} " +
        s"is not equal to ${DataEntry(Ints.toByteArray(1), DataType.Int32).json}")))

    val tx = RegisterContractTransaction.create(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
      ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
      "vsys", 10000L, 100, 1L)

    AssertOpcDiff.isCallerOrigin(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      DataEntry(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr,
        DataType.Address)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.isCallerOrigin(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      DataEntry(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr,
        DataType.ContractAccount)) should be (Left(ContractDataTypeMismatch))
    AssertOpcDiff.isCallerOrigin(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      DataEntry(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(1)).toAddress.bytes.arr,
        DataType.Address)) should be (Left(ContractInvalidCaller))

    AssertOpcDiff.isSignerOrigin(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      DataEntry(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr,
        DataType.Address)) should be (Right(OpcDiff.empty))
    AssertOpcDiff.isSignerOrigin(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      DataEntry(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr,
        DataType.ContractAccount)) should be (Left(ContractDataTypeMismatch))
    AssertOpcDiff.isSignerOrigin(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      DataEntry(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(1)).toAddress.bytes.arr,
        DataType.Address)) should be (Left(ContractInvalidSigner))
  }
}
