package vsys.blockchain.transaction

import vsys.utils.base58Length

import scala.util.{Failure, Try}

trait TransactionParser {
  def parseTail(data: Array[Byte]): Try[Transaction]
}

object TransactionParser {

  object TransactionType extends Enumeration {

    sealed case class TxTypeVal(txType: Int, txObj: TransactionParser) extends Val(txType) {
      def *(n: Int): Int = n * txType
    }

    val GenesisTransaction = TxTypeVal(1, vsys.blockchain.transaction.GenesisTransaction)
    val PaymentTransaction = TxTypeVal(2, vsys.blockchain.transaction.PaymentTransaction)
    val LeaseTransaction = TxTypeVal(3, vsys.blockchain.transaction.lease.LeaseTransaction)
    val LeaseCancelTransaction = TxTypeVal(4, vsys.blockchain.transaction.lease.LeaseCancelTransaction)
    val MintingTransaction = TxTypeVal(5, vsys.blockchain.transaction.MintingTransaction)
    val ContendSlotsTransaction = TxTypeVal(6, vsys.blockchain.transaction.spos.ContendSlotsTransaction)
    val ReleaseSlotsTransaction = TxTypeVal(7, vsys.blockchain.transaction.spos.ReleaseSlotsTransaction)
    val RegisterContractTransaction = TxTypeVal(8, vsys.blockchain.transaction.contract.RegisterContractTransaction)
    val ExecuteContractFunctionTransaction = TxTypeVal(9, vsys.blockchain.transaction.contract.ExecuteContractFunctionTransaction)
    val DbPutTransaction = TxTypeVal(10, vsys.blockchain.transaction.database.DbPutTransaction)

    def fromByte(implicit b: Byte): Option[TxTypeVal] = Try(TransactionType(b).asInstanceOf[TxTypeVal]).toOption
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
    data.headOption.flatMap(TransactionType.fromByte(_)) match {
      case Some(txType) => txType.txObj.parseTail(data.tail)
      case _ => Failure(new Exception(s"Invalid transaction type"))
    }
}
