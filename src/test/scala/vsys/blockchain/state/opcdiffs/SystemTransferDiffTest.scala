package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.blockchain.contract.{CallType, ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.{ByteStr, StateWriterImpl}
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidAmount,
  ContractTokenBalanceInsufficient, GenericError, InvalidContractAddress}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.{Left, Right}

class SystemTransferDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

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

  val account1: Array[Byte] = PrivateKeyAccount(
    Array.fill(TransactionParser.KeyLength)(1)).toAddress.bytes.arr

  property("test system transfer opcs") {
    SystemTransferDiff.getTriggerCallOpcDiff(executionContext, OpcDiff.empty, DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Longs.toByteArray(0), DataType.Amount), DataEntry(
        Ints.toByteArray(0), DataType.Int32), CallType.Trigger, 2) should be (
      Right(OpcDiff.empty))
    SystemTransferDiff.getTriggerCallOpcDiff(executionContext, OpcDiff.empty, DataEntry(
      ContractAccount.fromId(ByteStr(Array.emptyByteArray)).bytes.arr, DataType.ContractAccount),
      DataEntry(account1, DataType.Address),
      DataEntry(Longs.toByteArray(0), DataType.Amount), DataEntry(
        Ints.toByteArray(0), DataType.Int32), CallType.Trigger, 2) should be (
      Left(InvalidContractAddress))
    SystemTransferDiff.getTriggerCallOpcDiff(executionContext, OpcDiff.empty, DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Longs.toByteArray(0), DataType.Amount), DataEntry(
        Ints.toByteArray(0), DataType.Int32), CallType.Trigger, 1) should be (
      Right(OpcDiff.empty))
    SystemTransferDiff.getTriggerCallOpcDiff(executionContext, OpcDiff.empty,
      DataEntry(account1, DataType.Address),
      DataEntry(ContractAccount.fromId(ByteStr(Array.emptyByteArray)).bytes.arr, DataType.ContractAccount),
      DataEntry(Longs.toByteArray(0), DataType.Amount), DataEntry(
        Ints.toByteArray(0), DataType.Int32), CallType.Trigger, 1) should be (
      Left(InvalidContractAddress))
    SystemTransferDiff.getTriggerCallOpcDiff(executionContext, OpcDiff.empty, DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Longs.toByteArray(0), DataType.Amount), DataEntry(
        Ints.toByteArray(0), DataType.Int32), CallType.Trigger, 3) should be (
      Left(GenericError("Invalid Call Index")))
    SystemTransferDiff.getTriggerCallOpcDiff(executionContext, OpcDiff.empty, DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Longs.toByteArray(0), DataType.Amount), DataEntry(
        Ints.toByteArray(0), DataType.Int32), CallType.Function, 1) should be (
      Left(GenericError("Invalid Call Type")))

    SystemTransferDiff.transfer(executionContext)(DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Ints.toByteArray(0), DataType.Int32)) should be (
      Left(ContractDataTypeMismatch))
    SystemTransferDiff.transfer(executionContext)(DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Longs.toByteArray(1), DataType.Amount)) should be (
      Left(ContractTokenBalanceInsufficient))
    SystemTransferDiff.transfer(executionContext)(DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Longs.toByteArray(-1), DataType.Amount)) should be (
      Left(ContractInvalidAmount))
    SystemTransferDiff.transfer(executionContext)(DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Longs.toByteArray(0), DataType.Amount)).right.get.relatedAddress(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(1)).toAddress) shouldEqual true
    SystemTransferDiff.transfer(executionContext)(DataEntry(
      account, DataType.Address), DataEntry(
      account1, DataType.Address),
      DataEntry(Longs.toByteArray(0), DataType.Amount)).right.get.portfolios(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(1)).toAddress).balance shouldEqual 0
  }
}
