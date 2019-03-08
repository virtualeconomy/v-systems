package vsys.state.opcdiffs

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.{IssueTransaction, TransferTransaction}
import vsys.contract.{DataEntry, DataType}

import scala.util.{Left, Right}

object LoadOpcDiff {

  def issuer (dataStack: Seq[DataEntry])(tx: IssueTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // contractId ++ "issuer"
    val issuer = tx.sender.address
    dataStack ++ Seq(DataEntry(issuer.getBytes, DataType.Address))
    if (issuer.isEmpty)
      Right(dataStack)
    else
      Left(GenericError(s"Invalid Issuer"))
  }

  def sender (dataStack: Seq[DataEntry])(tx: TransferTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // contractId ++ "issuer"
    val sender = tx.sender.address
    dataStack ++ Seq(DataEntry(sender.getBytes, DataType.Address))
    if (sender.isEmpty)
      Right(dataStack)
    else
      Left(GenericError(s"Invalid Issuer"))
  }

  def max(tokenId: ByteStr, s: StateReader, dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID ++ ”max” -> DataEntry.bytes
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, Array("max".toByte)))
    val max = s.tokenInfo(id)
    max match {
      case Some(i) => dataStack ++ max
    }
    if (max.isEmpty)
      Right(
        dataStack
      )
    else
      Left(GenericError("Transaction cannot appear in invalid block"))
  }

  def total(s: StateReader, tokenId: ByteStr, dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID ++ ”total” -> DataEntry.bytes
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, Array("total".toByte)))
    val total = s.tokenInfo(id)
    total match {
      case Some(i) => dataStack ++ total
    }
    if (total.isEmpty)
      Right(
        dataStack
      )
    else
      Left(GenericError("Transaction cannot appear in invalid block"))
  }

  def issuerBalance(s: StateReader, tokenId: ByteStr, dataStack: Seq[DataEntry])(tx: IssueTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID + Address + "balance" -> Balance
    val senderAddress = tx.sender.address
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, senderAddress.getBytes, Array("balance".toByte)))
    val balance: Option[Long] = s.tokenAccountBalance(id)
    balance match {
      case Some(i) => dataStack ++ Seq(DataEntry(Longs.toByteArray(balance.asInstanceOf[Long]), DataType.Amount))
    }
    if (balance.asInstanceOf[Long] < 0)
      Right(
        dataStack
      )
    else
      Left(GenericError("Transaction cannot appear in invalid block"))
  }

  def senderBalance(s: StateReader, tokenId: ByteStr, dataStack: Seq[DataEntry])(tx: TransferTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID + Address + "balance" -> Balance
    val senderAddress = tx.sender.address
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, senderAddress.getBytes, Array("balance".toByte)))
    val balance: Option[Long] = s.tokenAccountBalance(id)
    balance match {
      case Some(i) => dataStack ++ Seq(DataEntry(Longs.toByteArray(balance.asInstanceOf[Long]), DataType.Amount))
    }
    if (balance.asInstanceOf[Long] < 0)
      Right(
        dataStack
      )
    else
      Left(GenericError("Transaction cannot appear in invalid block"))
  }

  def recipientBalance(s: StateReader, tokenId: ByteStr, dataStack: Seq[DataEntry])(tx: TransferTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID + Address + "balance" -> Balance
    val senderAddress = tx.recipient.stringRepr
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, senderAddress.getBytes, Array("balance".toByte)))
    val balance: Option[Long] = s.tokenAccountBalance(id)
    balance match {
      case Some(i) => dataStack ++ Seq(DataEntry(Longs.toByteArray(balance.asInstanceOf[Long]), DataType.Amount))
    }
    if (balance.asInstanceOf[Long] < 0)
      Right(
        dataStack
      )
    else
      Left(GenericError("Transaction cannot appear in invalid block"))
  }

}


