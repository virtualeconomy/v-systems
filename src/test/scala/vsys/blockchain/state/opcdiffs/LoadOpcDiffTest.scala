package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs, Shorts}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.StateWriterImpl
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.ValidationError.ContractLocalVariableIndexOutOfRange
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.{Left, Right}

class LoadOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  val state: StateWriterImpl = newState()

  val tx: RegisterContractTransaction = RegisterContractTransaction.create(
    PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
    ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
    "vsys", 10000L, 100, 1L).right.get

  val executionContext: ExecutionContext = ExecutionContext.fromRegConTx(
    state, TestFunctionalitySettings.Enabled, Option(0L),
    1L, 1, tx).right.get

  property("test load opcs") {

    LoadOpcDiff.signer(executionContext)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr, DataType.Address))))
    LoadOpcDiff.signer(executionContext)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.caller(executionContext)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).toAddress.bytes.arr, DataType.Address))))
    LoadOpcDiff.caller(executionContext)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.timestamp(executionContext)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      Longs.toByteArray(0), DataType.Timestamp))))
    LoadOpcDiff.timestamp(executionContext)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.lastTokenIndex(executionContext)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      Ints.toByteArray(-1), DataType.Int32))))
    LoadOpcDiff.lastTokenIndex(executionContext)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.transactionId(executionContext)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      Shorts.toByteArray(tx.id.arr.length.toShort) ++ tx.id.arr, DataType.ShortBytes))))
    LoadOpcDiff.transactionId(executionContext)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.signerPublicKey(executionContext)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)).publicKey, DataType.PublicKey))))
    LoadOpcDiff.signerPublicKey(executionContext)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))

    LoadOpcDiff.height(executionContext)(
      Seq.empty, 0) should be (Right(Seq(DataEntry(
      Ints.toByteArray(1), DataType.Int32))))
    LoadOpcDiff.height(executionContext)(
      Seq.empty, 1) should be (Left(ContractLocalVariableIndexOutOfRange))
  }
}
