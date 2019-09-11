package vsys.blockchain.transaction

import java.util

import com.google.common.primitives.Longs

import scala.util.{Failure, Try}

object ProcessedTransactionParser {

  private val FeeLength = 8

  def parseBytes(data: Array[Byte]): Try[ProcessedTransaction] = {
    require(data.length > FeeLength, "Data does not match base length")

    data.headOption.flatMap(f => Try(TransactionStatus(f)).toOption) match {
      case Some(status) =>
        parseTail(status, data.tail)

      case _ => Failure(new Exception(s"Invalid transaction status: ${data.headOption.getOrElse("None")}"))
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
