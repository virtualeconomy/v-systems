package vsys.transaction

import scala.util.{Failure, Try}
import java.util
import scorex.transaction.TransactionParser

import com.google.common.primitives.Longs

object ProcessedTransactionParser {

  private val FeeLength = 8

  def parseBytes(data: Array[Byte]): Try[ProcessedTransaction] = {
    require(data.length > FeeLength, "Data does not match base length")

    data.head match {
      case status: Byte if status == TransactionStatus.Success.id =>
        parseTail(TransactionStatus.Success, data.tail)

      case status: Byte if status == TransactionStatus.Failed.id =>
        parseTail(TransactionStatus.Failed, data.tail)

      case status: Byte if status == TransactionStatus.Unprocessed.id =>
        parseTail(TransactionStatus.Unprocessed, data.tail)

      case status: Byte if status == TransactionStatus.ContendFailed.id =>
        parseTail(TransactionStatus.ContendFailed, data.tail)

      case status: Byte if status == TransactionStatus.InvalidContract.id =>
        parseTail(TransactionStatus.InvalidContract, data.tail)

      case status: Byte if status == TransactionStatus.InvalidContractAddress.id =>
        parseTail(TransactionStatus.InvalidContractAddress, data.tail)

      case status: Byte if status == TransactionStatus.InvalidDataEntry.id =>
        parseTail(TransactionStatus.InvalidDataEntry, data.tail)

      case status: Byte if status == TransactionStatus.InvalidFunctionIndex.id =>
        parseTail(TransactionStatus.InvalidFunctionIndex, data.tail)

      case status: Byte if status == TransactionStatus.ContractDataTypeMissMatch.id =>
        parseTail(TransactionStatus.ContractDataTypeMissMatch, data.tail)

      case status: Byte if status == TransactionStatus.ContractInvalidStateVariable.id =>
        parseTail(TransactionStatus.ContractInvalidStateVariable, data.tail)

      case status: Byte if status == TransactionStatus.ContractStateVariableNotDefined.id =>
        parseTail(TransactionStatus.ContractStateVariableNotDefined, data.tail)

      case status: Byte if status == TransactionStatus.ContractInvalidOPCData.id =>
        parseTail(TransactionStatus.ContractInvalidOPCData, data.tail)

      case status: Byte if status == TransactionStatus.ContractUnsupportedOPC.id =>
        parseTail(TransactionStatus.ContractUnsupportedOPC, data.tail)

      case status: Byte if status == TransactionStatus.ContractInvalidFunction.id =>
        parseTail(TransactionStatus.ContractInvalidFunction, data.tail)

      case status: Byte if status == TransactionStatus.ContractInvalidTokenIndex.id =>
        parseTail(TransactionStatus.ContractInvalidTokenIndex, data.tail)

      case status: Byte if status == TransactionStatus.ContractInvalidAmount.id =>
        parseTail(TransactionStatus.ContractInvalidAmount, data.tail)

      case status: Byte if status == TransactionStatus.ContractLocalVariableIndexOutOfRange.id =>
        parseTail(TransactionStatus.ContractLocalVariableIndexOutOfRange, data.tail)

      case status: Byte if status == TransactionStatus.ContractTokenBalanceInsufficient.id =>
        parseTail(TransactionStatus.ContractTokenBalanceInsufficient, data.tail)

      case status: Byte if status == TransactionStatus.ContractTokenMaxExceeded.id =>
        parseTail(TransactionStatus.ContractTokenMaxExceeded, data.tail)

      case status: Byte if status == TransactionStatus.ContractInvalidTokenInfo.id =>
        parseTail(TransactionStatus.ContractInvalidTokenInfo, data.tail)

      case status: Byte if status == TransactionStatus.ContractUnsupportedWithdraw.id =>
        parseTail(TransactionStatus.ContractUnsupportedWithdraw, data.tail)

      case status: Byte if status == TransactionStatus.ContractUnsupportedDeposit.id =>
        parseTail(TransactionStatus.ContractUnsupportedDeposit, data.tail)

      case status => Failure(new Exception(s"Invalid transaction status: $status"))
    }
  }

  def parseTail(status: TransactionStatus.Value, data: Array[Byte]): Try[ProcessedTransaction] = {
    //READ FEE
    val feeBytes = util.Arrays.copyOfRange(data, 0, FeeLength)
    val fee = Longs.fromByteArray(feeBytes)

    TransactionParser.parseBytes(util.Arrays.copyOfRange(data, FeeLength, data.length)).map(
      ProcessedTransaction(status, fee, _)
    )
  }
}
