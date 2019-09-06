package vsys.blockchain.transaction

import vsys.utils.base58Length
import vsys.blockchain.transaction.contract.{ExecuteContractFunctionTransaction, RegisterContractTransaction}
import vsys.blockchain.transaction.database.DbPutTransaction
import vsys.blockchain.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import vsys.blockchain.transaction.spos.{ContendSlotsTransaction, ReleaseSlotsTransaction}

import scala.util.{Failure, Try}

trait TransactionParser {
  def parseTail[T <: Transaction](data: Array[Byte]): Try[T]
}

object TransactionParser {

  object TransactionType extends Enumeration {

    sealed case class TxTypeVal(txType: Int, txObj: TransactionParser) extends Val(txType) {
      def *(n: Int): Int = n * txType
    }

    val GenesisTransaction = TxTypeVal(1, transaction.GenesisTransaction)
    val PaymentTransaction = TxTypeVal(2, transaction.PaymentTransaction)
    val LeaseTransaction = TxTypeVal(3, transaction.LeaseTransaction)
    val LeaseCancelTransaction = TxTypeVal(4, transaction.LeaseCancelTransaction)
    val MintingTransaction = TxTypeVal(5, transaction.MintingTransaction)
    val ContendSlotsTransaction = TxTypeVal(6, transaction.ContendSlotsTransaction)
    val ReleaseSlotsTransaction = TxTypeVal(7, transaction.ReleaseSlotsTransaction)
    val RegisterContractTransaction = TxTypeVal(8, transaction.RegisterContractTransaction)
    val ExecuteContractFunctionTransaction = TxTypeVal(9, transaction.ExecuteContractFunctionTransaction)
    val DbPutTransaction = TxTypeVal(10, transaction.DbPutTransaction)

    def fromByte(implicit b: Byte): Option[TxTypeVal] {
      Try(TransactionType(b).asInstanceOf[TxTypeVal]).toOption
    }
  }

  val TimestampLength = 8
  val AmountLength = 8
  val TypeLength = 1
  val SignatureLength = 64
  val SignatureStringLength: Int = base58Length(SignatureLength)
  val KeyLength = 32
  val SlotIdLength = 4
  val DefaultFeeScale: Short = 100
  val KeyStringLength: Int = base58Length(KeyLength)

  def parseBytes(data: Array[Byte]): Try[Transaction] =
    data.headOption.flatMap(TransactionType.fromByte) match {
      case Some(txType: TransactionType.TxTypeVal): txType.txObj.parseTail(data.tail)
      case _ => Failure(new Exception(s"Invalid transaction type: $txType"))
    }
}
