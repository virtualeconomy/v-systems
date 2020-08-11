package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs, Shorts}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.ValidationError.ContractLocalVariableIndexOutOfRange
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.{Left, Right}

class LoadOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("test load opcs") {

    val state = newState()

    val tx = RegisterContractTransaction.create(PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
      ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
      "vsys", 10000L, 100, 1L)

    LoadOpcDiff.signer(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr, DataType.Address))))
    LoadOpcDiff.signer(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.caller(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr, DataType.Address))))
    LoadOpcDiff.caller(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.timestamp(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      Longs.toByteArray(0), DataType.Timestamp))))
    LoadOpcDiff.timestamp(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.lastTokenIndex(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      Ints.toByteArray(-1), DataType.Int32))))
    LoadOpcDiff.lastTokenIndex(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.transactionId(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      Shorts.toByteArray(tx.right.get.id.arr.length.toShort) ++ tx.right.get.id.arr, DataType.ShortBytes))))
    LoadOpcDiff.transactionId(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.signerPublicKey(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).publicKey, DataType.PublicKey))))
    LoadOpcDiff.signerPublicKey(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.height(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      Ints.toByteArray(1), DataType.Int32))))
    LoadOpcDiff.height(ExecutionContext.fromRegConTx(state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx.right.get).right.get)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
  }
}
