package vsys.account

import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.ValidationError


trait Account {
  def stringRepr: String

  def bytes: ByteStr

  override def toString: String = stringRepr

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: Account => bytes == a.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}

object Account {

  def fromBytes(bytes: Array[Byte], position: Int): Either[ValidationError, (Account, Int)] = {
    bytes(position) match {
      case Address.AddressVersion =>
        val addressEnd = position + Address.AddressLength
        val addressBytes = bytes.slice(position, addressEnd)
        Address.fromBytes(addressBytes).map((_, addressEnd))
      case ContractAccount.AddressVersion =>
        val contractAccountEnd = position + ContractAccount.AddressLength
        val contractAccountBytes = bytes.slice(position, contractAccountEnd)
        ContractAccount.fromBytes(contractAccountBytes).map((_, contractAccountEnd))
      case _ => Left(ValidationError.InvalidAddress)
    }
  }
}