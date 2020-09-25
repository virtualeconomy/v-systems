package vsys.blockchain.contract

import vsys.blockchain.transaction.TransactionParser.{AmountLength, KeyLength, TimestampLength}
import play.api.libs.json.{JsValue, Json}
import com.google.common.primitives.{Ints, Longs, Shorts}
import vsys.account.{AddressScheme, PublicKeyAccount}
import scorex.crypto.encode.Base58
import vsys.account.ContractAccount.ChecksumLength
import vsys.blockchain.state.ByteStr
import vsys.utils.crypto.hash.SecureCryptographicHash._
import scala.util.Try

object DataType extends Enumeration {

  val MaxShortTextLength = 140
  val MaxShortBytesLength = 255
  val MaxOpcBlockLength = 255
  val MaxBigIntLength = 255       // less than 1024 bits

  sealed case class DataTypeVal[T](dataType: Int, lenFixed: Boolean, maxLen: Int,
                                   deserializer: Array[Byte] => T,
                                   serializer: T => Array[Byte], // WITHOUT length prefix
                                   jsonifier: T => JsValue,
                                   private val extValidator: Array[Byte] => Boolean) extends Val(dataType) {

    def *(n: Int): Int = n * dataType

    val jsonifierB: Array[Byte] => JsValue = b => jsonifier(deserializer(b))

    // check length and other limits
    val validator: Array[Byte] => Boolean = data => (((lenFixed && data.length == maxLen)
        || (!lenFixed && data.length >= 2 && Shorts.fromByteArray(data.take(2)) == data.drop(2).length && data.drop(2).length <= maxLen))
      && extValidator(data))
  }


  val DataTypeObj     = DataTypeVal[DataTypeVal[_]](0,
    lenFixed          = true,
    maxLen            = 1,
    deserializer      = x => fromByte(x(1)).get,
    serializer        = v => Array(v.id.toByte),
    jsonifier         = v => Json.toJson(v.toString),
    extValidator      = x => fromByte(x(1)).nonEmpty)

  val PublicKey       = DataTypeVal[PublicKeyAccount](1,
    lenFixed          = true,
    maxLen            = KeyLength,
    deserializer      = PublicKeyAccount(_),
    serializer        = p => p.publicKey,
    jsonifier         = p => Json.toJson(p.address),
    extValidator      = _ => true)

  val Address         = DataTypeVal[vsys.account.Address](2,
    lenFixed          = true,
    maxLen            = vsys.account.Address.AddressLength,
    deserializer      = vsys.account.Address.fromBytes(_).right.get,
    serializer        = a => a.bytes.arr,
    jsonifier         = a => Json.toJson(a.address),
    extValidator      = vsys.account.Address.fromBytes(_).isRight)

  val Amount          = DataTypeVal[Long](3,
    lenFixed          = true,
    maxLen            = AmountLength,
    deserializer      = Longs.fromByteArray(_),
    serializer        = Longs.toByteArray(_),
    jsonifier         = Json.toJson(_),
    extValidator      = Longs.fromByteArray(_) >= 0)

  val Int32           = DataTypeVal[Int](4,
    lenFixed          = true,
    maxLen            = 4,
    deserializer      = Ints.fromByteArray(_),
    serializer        = Ints.toByteArray(_),
    jsonifier         = Json.toJson(_),
    extValidator      = Ints.fromByteArray(_) >= 0)

  val ShortText       = DataTypeVal[String](5,
    lenFixed          = false,
    maxLen            = MaxShortTextLength,
    deserializer      = b => new String(b.drop(2)),
    serializer        = s => arrayShortLengthToByteArray(s.getBytes) ++ s.getBytes,
    jsonifier         = Json.toJson(_),
    extValidator      = _ => true)

  val ContractAccount = DataTypeVal[vsys.account.ContractAccount](6,
    lenFixed          = true,
    maxLen            = vsys.account.ContractAccount.AddressLength,
    deserializer      = vsys.account.ContractAccount.fromBytes(_).right.get,
    serializer        = a => a.bytes.arr,
    jsonifier         = a => Json.toJson(a.address),
    extValidator      = vsys.account.ContractAccount.fromBytes(_).isRight)

  val Account         = DataTypeVal[vsys.account.Account](7,
    lenFixed          = false,
    maxLen            = (vsys.account.Address.AddressLength).max(vsys.account.ContractAccount.AddressLength),
    deserializer      = vsys.account.Account.fromBytes(_, 0).right.get._1,
    serializer        = a => a.bytes.arr,
    jsonifier         = a => Json.toJson(a.bytes.arr),
    extValidator      = _ => false)  // unsupported type for now. vsys.account.Account.fromBytes(_, 0).isRight)

  val TokenId         = DataTypeVal[ByteStr](8,
    lenFixed          = true,
    maxLen            = vsys.account.ContractAccount.TokenAddressLength,
    deserializer      = ByteStr(_),
    serializer        = b => b.arr,
    jsonifier         = b => Json.toJson(b.base58),
    extValidator      = addressBytes => {
      // length is already ensured here
      def scheme    = AddressScheme.current
      val version   = addressBytes.head
      val network   = addressBytes.tail.head
      val checkSum1 = addressBytes.takeRight(ChecksumLength)
      val checkSum2 = hash((addressBytes.dropRight(ChecksumLength))).take(ChecksumLength)
      // addr version && net id && checksum
      version == vsys.account.ContractAccount.TokenAddressVersion && network == scheme.chainId && checkSum1.sameElements(checkSum2)
    })

  val Timestamp       = DataTypeVal[Long](9,
    lenFixed          = true,
    maxLen            = TimestampLength,
    deserializer      = Longs.fromByteArray(_),
    serializer        = Longs.toByteArray(_),
    jsonifier         = Json.toJson(_),
    extValidator      = Longs.fromByteArray(_) >= 0)

  val Boolean         = DataTypeVal[Boolean](10,
    lenFixed          = true,
    maxLen            = 1,
    deserializer      = b => b.head == 1,
    serializer        = b => Array((if(b) 1 else 0).toByte),
    jsonifier         = b => Json.toJson(b.toString),
    extValidator      = b => (b(0) == 1 || b(0) == 0))

  val ShortBytes      = DataTypeVal[Array[Byte]](11,
    lenFixed          = false,
    maxLen            = MaxShortBytesLength,
    deserializer      = b => b.drop(2),
    serializer        = b => arrayShortLengthToByteArray(b) ++ b,
    jsonifier         = b => Json.toJson(Base58.encode(b)),
    extValidator      = _ => true)

  val Balance         = DataTypeVal[Long](12,
    lenFixed          = true,
    maxLen            = AmountLength,
    deserializer      = Longs.fromByteArray(_),
    serializer        = Longs.toByteArray(_),
    jsonifier         = Json.toJson(_),
    extValidator      = _ => false) // unsupported type for now. Longs.fromByteArray(_) >= 0)

  val OpcBlock        = DataTypeVal[Array[Byte]](13,
    lenFixed          = false,
    maxLen            = MaxOpcBlockLength,
    deserializer      = b => b.drop(2),
    serializer        = b => arrayShortLengthToByteArray(b) ++ b,
    jsonifier         = b => Json.toJson(Base58.encode(b)),
    extValidator      = _ => true)

  val BigInteger      = DataTypeVal[BigInt](14,
    lenFixed          = false,
    maxLen            = MaxBigIntLength,
    deserializer      = b => BigInt(b.drop(2)),
    serializer        = i => arrayShortLengthToByteArray(i.toByteArray) ++ i.toByteArray,
    jsonifier         = i => Json.toJson(i.toString),
    extValidator      = _ => true)

  def fromByte(b: Byte): Option[DataTypeVal[_]] = Try(DataType(b).asInstanceOf[DataTypeVal[_]]).toOption
  
  def check(a: Byte, b: Byte): Boolean = {
    if (a == b) true
    else if (a == DataType.Account.id) b == DataType.Address.id || b == DataType.ContractAccount.id
    else if (b == DataType.Account.id) check(b, a)
    else false
  }

  def checkTypes(paraTypes: Array[Byte], dataTypes: Array[Byte]): Boolean = {
    paraTypes.length == dataTypes.length && (paraTypes, dataTypes).zipped.forall { case (a, b) => check(a, b) }
  }

  def arrayShortLengthToByteArray(a: Array[_]) = Shorts.toByteArray(a.length.toShort)

}
