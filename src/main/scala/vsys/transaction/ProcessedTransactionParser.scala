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

      case status: Byte if status == TransactionStatus.RegisterContractFailed.id =>
        parseTail(TransactionStatus.RegisterContractFailed, data.tail)

      case status: Byte if status == TransactionStatus.ExecuteContractFunctionFailed.id =>
        parseTail(TransactionStatus.ExecuteContractFunctionFailed, data.tail)

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
