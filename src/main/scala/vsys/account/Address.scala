package vsys.account

import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.InvalidAddress
import scorex.crypto.encode.Base58
import vsys.utils.crypto.hash.SecureCryptographicHash._
import vsys.utils.{base58Length, ScorexLogging}

import scala.util.Success

sealed trait Address extends Account with Serializable {

  val bytes: ByteStr
  lazy val address: String = bytes.base58
  lazy val stringRepr: String = address

  override def toString: String = stringRepr

  override def equals(obj: Any): Boolean = obj match {
    case a: Address => bytes == a.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}

object Address extends ScorexLogging {

  val Prefix: String = "address:"

  val AddressVersion: Byte = 5
  val ChecksumLength = 4
  val HashLength = 20
  val AddressLength = 1 + 1 + ChecksumLength + HashLength
  val AddressStringLength = base58Length(AddressLength)

  private def scheme = AddressScheme.current.value

  private class AddressImpl(val bytes: ByteStr) extends Address with Serializable

  def fromPublicKey(publicKey: Array[Byte]): Address = {
    val publicKeyHash = hash(publicKey).take(HashLength)
    val withoutChecksum = AddressVersion +: scheme.chainId +: publicKeyHash
    val bytes = withoutChecksum ++ calcCheckSum(withoutChecksum)
    new AddressImpl(ByteStr(bytes))
  }

  def fromBytes(addressBytes: Array[Byte]): Either[ValidationError, Address] = {
    if (isByteArrayValid(addressBytes)) Right(new AddressImpl(ByteStr(addressBytes)))
    else Left(InvalidAddress)
  }

  private def fromBase58String(address: String): Either[ValidationError, Address] =
    if (address.length > AddressStringLength) {
      Left(InvalidAddress)
    } else {
      Base58.decode(address) match {
        case Success(byteArray) if isByteArrayValid(byteArray) => Right(new AddressImpl(ByteStr(byteArray)))
        case _ => Left(InvalidAddress)
      }
    }

  def fromString(address: String): Either[ValidationError, Address] = {
    val base58String = if (address.startsWith(Prefix))
      address.drop(Prefix.length)
    else address
    fromBase58String(base58String)
  }

  def fromBytes(bytes: Array[Byte], position: Int): Either[ValidationError, (Address, Int)] = {
    bytes(position) match {
      case Address.AddressVersion =>
        val addressEnd = position + Address.AddressLength
        val addressBytes = bytes.slice(position, addressEnd)
        Address.fromBytes(addressBytes).map((_, addressEnd))
      case _ => Left(ValidationError.InvalidAddress)
    }
  }

  private def isByteArrayValid(addressBytes: Array[Byte]): Boolean = {
    val version = addressBytes.head
    val network = addressBytes.tail.head
    if (version != AddressVersion) {
      log.warn(s"Unknown address version: $version")
      false
    } else if (network != scheme.chainId) {
      log.warn(s"~ Expected network: ${scheme.chainId}(${scheme.chainId.toChar}")
      log.warn(s"~ Actual network: $network(${network.toChar}")
      false
    } else {
      if (addressBytes.length != Address.AddressLength)
        false
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)
        val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))
        checkSum.sameElements(checkSumGenerated)
      }
    }
  }

  private def calcCheckSum(withoutChecksum: Array[Byte]): Array[Byte] = hash(withoutChecksum).take(ChecksumLength)

}
