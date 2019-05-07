package vsys.account

import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import scorex.account.AddressScheme
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.{InvalidContractAddress, InvalidAddress}
import scorex.utils.ScorexLogging

import scala.util.Success

sealed trait ContractAccount extends Serializable {

  val bytes: ByteStr
  lazy val address: String = bytes.base58
  lazy val stringRepr: String = address

  override def toString: String = stringRepr

  override def equals(obj: scala.Any): Boolean = obj match {
    case conAcc: ContractAccount => bytes == conAcc.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)

}

object ContractAccount extends ScorexLogging {

  val Prefix: String = "contractAccount:"

  val AddressVersion: Byte = 6
  val TokenAddressVersion: Byte = -124
  val ChecksumLength = 4
  val HashLength = 20
  val AddressLength = 1 + 1 + ChecksumLength + HashLength
  val AddressStringLength = base58Length(AddressLength)

  private def scheme = AddressScheme.current

  private class ContractAddressImpl(val bytes: ByteStr) extends ContractAccount

  def fromId(id: ByteStr): ContractAccount = {
    val contractAccountHash = hash(id.arr).take(HashLength)
    val withoutChecksum = AddressVersion +: scheme.chainId +: contractAccountHash
    val bytes = withoutChecksum ++ calcCheckSum(withoutChecksum)
    new ContractAddressImpl(ByteStr(bytes))
  }

  def fromBytes(addressBytes: Array[Byte]): Either[ValidationError, ContractAccount] = {
    if (isByteArrayValid(addressBytes)) Right(new ContractAddressImpl(ByteStr(addressBytes)))
    else Left(InvalidContractAddress)
  }

  private def fromBase58String(address: String): Either[ValidationError, ContractAccount] = {
    if (address.length > AddressStringLength) Left(InvalidContractAddress)
    else {
      Base58.decode(address) match {
        case Success(byteArray) if isByteArrayValid(byteArray) => Right(new ContractAddressImpl(ByteStr(byteArray)))
        case _ => Left(InvalidContractAddress)
      }
    }
  }

  def fromString(address: String): Either[ValidationError, ContractAccount] = {
    val base58String = if (address.startsWith(Prefix))
      address.drop(Prefix.length)
    else address
    fromBase58String(base58String)
  }

  private def isByteArrayValid(addressBytes: Array[Byte]): Boolean = {
    val version = addressBytes.head
    val network = addressBytes.tail.head
    if (version != AddressVersion) {
      log.warn(s"Unknown contract address version: $version")
      false
    } else if (network != scheme.chainId) {
      log.warn(s"~ Expected network: ${scheme.chainId}(${scheme.chainId.toChar}")
      log.warn(s"~ Actual network: $network(${network.toChar}")
      false
    } else {
      if (addressBytes.length != ContractAccount.AddressLength)
        false
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)
        val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))
        checkSum.sameElements(checkSumGenerated)
      }
    }
  }

  private def calcCheckSum(withoutChecksum: Array[Byte]): Array[Byte] = hash(withoutChecksum).take(ChecksumLength)

  def tokenIdFromBytes(addressBytes: Array[Byte], idxBytes: Array[Byte]): Either[ValidationError, ByteStr] = {
    if (isByteArrayValid(addressBytes)) {
      val contractIdNoCheckSum = addressBytes.tail.dropRight(ChecksumLength)
      val withoutChecksum = Array(TokenAddressVersion) ++ contractIdNoCheckSum ++ idxBytes
      val bytes = withoutChecksum ++ calcCheckSum(withoutChecksum)
      Right(ByteStr(bytes))
    } else Left(InvalidAddress)
  }

}
