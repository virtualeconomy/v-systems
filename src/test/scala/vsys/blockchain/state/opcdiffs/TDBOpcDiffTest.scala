package vsys.blockchain.state.opcdiffs

import com.google.common.primitives.{Bytes, Ints, Longs}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.account.PrivateKeyAccount
import vsys.blockchain.contract.{ContractPermitted, DataEntry, DataType, ExecutionContext}
import vsys.blockchain.state.{ByteStr, StateWriterImpl}
import vsys.blockchain.state.diffs.newState
import vsys.blockchain.transaction.TransactionParser
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidTokenIndex, ContractInvalidTokenInfo}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.settings.TestFunctionalitySettings

import scala.util.Left

class TDBOpcDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("test TDB opcs") {

    val state: StateWriterImpl = newState()

    val tx: RegisterContractTransaction = RegisterContractTransaction.create(
      PrivateKeyAccount(Array.fill(TransactionParser.KeyLength)(0)),
      ContractPermitted.contract, Seq(DataEntry(Longs.toByteArray(-1), DataType.Amount)),
      "vsys", 10000L, 100, 1L).right.get

    val executionContext: ExecutionContext = ExecutionContext.fromRegConTx(
      state, TestFunctionalitySettings.Enabled, Option(0L),
      1L, 1, tx).right.get

    val tokenID: ByteStr = tokenIdFromBytes(executionContext.contractId.bytes.arr, Ints.toByteArray(0)).right.get

    TDBOpcDiff.newToken(executionContext)(DataEntry(
      Ints.toByteArray(0), DataType.Int32), DataEntry(
      Longs.toByteArray(0), DataType.Amount), DataEntry(
      Array.fill(13){1}, DataType.ShortText)) should be (Left(ContractDataTypeMismatch))
    TDBOpcDiff.newToken(executionContext)(DataEntry(
      Longs.toByteArray(0), DataType.Amount), DataEntry(
      Longs.toByteArray(0), DataType.Amount), DataEntry(
      Array.fill(13){1}, DataType.ShortText)) should be (Left(ContractInvalidTokenInfo))
    TDBOpcDiff.newToken(executionContext)(DataEntry(
      Longs.toByteArray(-1), DataType.Amount), DataEntry(
      Longs.toByteArray(1), DataType.Amount), DataEntry(
      Array.fill(13){1}, DataType.ShortText)) should be (Left(ContractInvalidTokenInfo))
    TDBOpcDiff.newToken(executionContext)(DataEntry(
      Longs.toByteArray(0), DataType.Amount), DataEntry(
      Longs.toByteArray(1), DataType.Amount), DataEntry(
      Array.fill(13){1}, DataType.ShortText)).right.get.tokenDB(
      ByteStr(Bytes.concat(tokenID.arr, Array(0.toByte)))) shouldEqual DataEntry(
      Longs.toByteArray(0), DataType.Amount).bytes

    TDBOpcDiff.split(executionContext)(DataEntry(
      Longs.toByteArray(0), DataType.Amount)) should be (Left(ContractInvalidTokenIndex))
    TDBOpcDiff.split(executionContext)(DataEntry(
      Ints.toByteArray(0), DataType.Int32)) should be (Left(ContractDataTypeMismatch))
  }
}
