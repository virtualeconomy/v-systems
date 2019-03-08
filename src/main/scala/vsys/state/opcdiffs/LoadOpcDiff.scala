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
    if (issuer != null) {
      dataStack ++ Seq(DataEntry(issuer.getBytes, DataType.Address))
      Right(dataStack)
    }
    else
      Left(GenericError(s"Invalid Issuer"))
  }

  def sender (dataStack: Seq[DataEntry])(tx: TransferTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // contractId ++ "issuer"
    val sender = tx.sender.address
    if (sender != null) {
      dataStack ++ Seq(DataEntry(sender.getBytes, DataType.Address))
      Right(dataStack)
    }
    else
      Left(GenericError(s"Invalid Sender"))
  }

  def max(tokenId: ByteStr, s: StateReader, dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID ++ ”max” -> DataEntry.bytes
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, Array("max".toByte)))
    val max = s.tokenInfo(id)
    max match {
      case Some(i) => Right(dataStack ++ max)
      case None => Left(GenericError("Transaction cannot appear in invalid block"))
    }
  }

  def total(s: StateReader, tokenId: ByteStr, dataStack: Seq[DataEntry]): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID ++ ”total” -> DataEntry.bytes
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, Array("total".toByte)))
    val total = s.tokenInfo(id)
    total match {
      case Some(i) => Right(dataStack ++ total)
      case None => Left(GenericError("Transaction cannot appear in invalid block"))
    }
  }

  def issuerBalance(s: StateReader, tokenId: ByteStr, dataStack: Seq[DataEntry])(tx: IssueTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID + Address + "balance" -> Balance
    val senderAddress = tx.sender.address
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, senderAddress.getBytes, Array("balance".toByte)))
    val balance: Option[Long] = s.tokenAccountBalance(id)
    balance match {
      case Some(i) => Right(dataStack ++ Seq(DataEntry(Longs.toByteArray(balance.asInstanceOf[Long]), DataType.Amount)))
      case None => Left(GenericError("Transaction cannot appear in invalid block"))
    }
  }

  def senderBalance(s: StateReader, tokenId: ByteStr, dataStack: Seq[DataEntry])(tx: TransferTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID + Address + "balance" -> Balance
    val senderAddress = tx.sender.address
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, senderAddress.getBytes, Array("balance".toByte)))
    val balance: Option[Long] = s.tokenAccountBalance(id)
    balance match {
      case Some(i) => Right(dataStack ++ Seq(DataEntry(Longs.toByteArray(balance.asInstanceOf[Long]), DataType.Amount)))
      case None => Left(GenericError("Transaction cannot appear in invalid block"))
    }
  }

  def recipientBalance(s: StateReader, tokenId: ByteStr, dataStack: Seq[DataEntry])(tx: TransferTransaction): Either[ValidationError, Seq[DataEntry]] = {
    // tokenID + Address + "balance" -> Balance
    val senderAddress = tx.recipient.stringRepr
    val id: ByteStr = ByteStr(Bytes.concat(tokenId.arr, senderAddress.getBytes, Array("balance".toByte)))
    val balance: Option[Long] = s.tokenAccountBalance(id)
    balance match {
      case Some(i) => Right(dataStack ++ Seq(DataEntry(Longs.toByteArray(balance.asInstanceOf[Long]), DataType.Amount)))
      case None => Left(GenericError("Transaction cannot appear in invalid block"))
    }
  }
}


