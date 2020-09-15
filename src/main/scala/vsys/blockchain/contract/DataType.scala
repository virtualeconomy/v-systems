package vsys.blockchain.contract

import vsys.blockchain.transaction.TransactionParser.{AmountLength, KeyLength, TimestampLength}

import scala.util.Try

object DataType extends Enumeration {

  val MaxShortTextLength = 140
  val MaxShortBytesLength = 255
  val MaxOpcBlockLength = 255

  sealed case class DataTypeVal(dataType: Int, lenFixed: Boolean, maxLen: Int) extends Val(dataType) { def *(n: Int): Int = n * dataType }

  val PublicKey = DataTypeVal(1, true, KeyLength)
  val Address = DataTypeVal(2, true, vsys.account.Address.AddressLength)
  val Amount = DataTypeVal(3, true, AmountLength)
  val Int32 = DataTypeVal(4, true, 4)
  val ShortText = DataTypeVal(5, false, MaxShortTextLength)
  val ContractAccount = DataTypeVal(6, true, vsys.account.ContractAccount.AddressLength)
  val Account = DataTypeVal(7, false, (vsys.account.Address.AddressLength).max(vsys.account.ContractAccount.AddressLength))
  val TokenId = DataTypeVal(8, true, vsys.account.ContractAccount.TokenAddressLength)
  val Timestamp = DataTypeVal(9, true, TimestampLength)
  val Boolean = DataTypeVal(10, true, 1)
  val ShortBytes = DataTypeVal(11, false, MaxShortBytesLength)
  val Balance = DataTypeVal(12, true, AmountLength)
  val OpcBlock = DataTypeVal(13, false, MaxOpcBlockLength)

  def fromByte(b: Byte): Option[DataType.DataTypeVal] = Try(DataType(b).asInstanceOf[DataTypeVal]).toOption
  
  private def check(a: Byte, b: Byte): Boolean = {
    if (a == b) true
    else if (a == DataType.Account.id) b == DataType.Address.id || b == DataType.ContractAccount.id
    else if (b == DataType.Account.id) check(b, a)
    else false
  }

  def checkTypes(paraTypes: Array[Byte], dataTypes: Array[Byte]): Boolean = {
    paraTypes.length == dataTypes.length && (paraTypes, dataTypes).zipped.forall { case (a, b) => check(a, b) }
  }

}
