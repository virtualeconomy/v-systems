package scorex.transaction

import com.wavesplatform.utils.base58Length
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import vsys.transaction.contract.{ChangeContractStatusTransaction, CreateContractTransaction}
import vsys.transaction.database.DbPutTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.transaction.MintingTransaction
import vsys.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}

import scala.util.{Failure, Try}

object TransactionParser {

  object TransactionType extends Enumeration {
    val GenesisTransaction = Value(1)
    val PaymentTransaction = Value(2)
    val LeaseTransaction = Value(3)
    val LeaseCancelTransaction = Value(4)
    val MintingTransaction = Value(5)
    val ContendSlotsTransaction = Value(6)
    val ReleaseSlotsTransaction = Value(7)
    val CreateContractTransaction = Value(8)
    val ChangeContractStatusTransaction = Value(9)
    val DbPutTransaction = Value(10)
    val IssueTransaction = Value(11)
    val TransferTransaction = Value(12)
    val ReissueTransaction = Value(13)
    val BurnTransaction = Value(14)
    val ExchangeTransaction = Value(15)
    val CreateAliasTransaction = Value(16)
  }

  val TimestampLength = 8
  val AmountLength = 8
  val TypeLength = 1
  val SignatureLength = 64
  val SignatureStringLength: Int = base58Length(SignatureLength)
  val KeyLength = 32
  val SlotIdLength = 4
  val KeyStringLength: Int = base58Length(KeyLength)

  def parseBytes(data: Array[Byte]): Try[Transaction] =
    data.head match {
      case txType: Byte if txType == TransactionType.GenesisTransaction.id =>
        GenesisTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.PaymentTransaction.id =>
        PaymentTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.IssueTransaction.id =>
        IssueTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.TransferTransaction.id =>
        TransferTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.ReissueTransaction.id =>
        ReissueTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.BurnTransaction.id =>
        BurnTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.ExchangeTransaction.id =>
        ExchangeTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.LeaseTransaction.id =>
        LeaseTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.LeaseCancelTransaction.id =>
        LeaseCancelTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.CreateAliasTransaction.id =>
        CreateAliasTransaction.parseTail(data.tail)
      
      case txType: Byte if txType == TransactionType.MintingTransaction.id =>
        MintingTransaction.parseTail(data.tail)
      
      case txType: Byte if txType == TransactionType.ContendSlotsTransaction.id =>
        ContendSlotsTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.ReleaseSlotsTransaction.id =>
        ReleaseSlotsTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.CreateContractTransaction.id =>
        CreateContractTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.ChangeContractStatusTransaction.id =>
        ChangeContractStatusTransaction.parseTail(data.tail)

      case txType: Byte if txType == TransactionType.DbPutTransaction.id =>
        DbPutTransaction.parseTail(data.tail)

      case txType => Failure(new Exception(s"Invalid transaction type: $txType"))
    }
}
