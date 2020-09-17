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
                                   serializer: T => Array[Byte],
                                   jsonifier: T => JsValue,
                                   private val extValidator: Array[Byte] => Boolean) extends Val(dataType) {

    def *(n: Int): Int = n * dataType

    val jsonifierB: Array[Byte] => JsValue = b => jsonifier(deserializer(b))

    // check length and other limits
    val validator: Array[Byte] => Boolean = data => (((lenFixed && data.length == maxLen)
        || (!lenFixed && data.length > 2 && Shorts.fromByteArray(data.take(2)) == data.drop(2).length && data.drop(2).length <= maxLen))
      && extValidator(data))
  }

  val PublicKey       = DataTypeVal(1,
    lenFixed          = true,
    maxLen            = KeyLength,
    deserializer      = PublicKeyAccount(_),
    serializer        = (p: PublicKeyAccount) => p.publicKey,
    jsonifier         = (p: PublicKeyAccount) => Json.toJson(p.address),
    extValidator      = _ => true)

  val Address         = DataTypeVal(2,
    lenFixed          = true,
    maxLen            = vsys.account.Address.AddressLength,
    deserializer      = vsys.account.Address.fromBytes(_).right.get,
    serializer        = (a: vsys.account.Address) => a.bytes.arr,
    jsonifier         = (a: vsys.account.Address) => Json.toJson(a.address),
    extValidator      = vsys.account.Address.fromBytes(_).isRight)

  val Amount          = DataTypeVal(3,
    lenFixed          = true,
    maxLen            = AmountLength,
    deserializer      = Longs.fromByteArray(_),
    serializer        = Longs.toByteArray(_),
    jsonifier         = (a: Long) => Json.toJson(a),
    extValidator      = Longs.fromByteArray(_) >= 0)

  val Int32           = DataTypeVal(4,
    lenFixed          = true,
    maxLen            = 4,
    deserializer      = Ints.fromByteArray(_),
    serializer        = Ints.toByteArray(_),
    jsonifier         = (i: Int) => Json.toJson(i),
    extValidator      = Ints.fromByteArray(_) >= 0)

  val ShortText       = DataTypeVal(5,
    lenFixed          = false,
    maxLen            = MaxShortTextLength,
    deserializer      = b => new String(b.drop(2)),
    serializer        = (s: String) => arrayShortLengthToByteArray(s.getBytes) ++ s.getBytes,
    jsonifier         = (s: String) => Json.toJson(s),
    extValidator      = _ => true)

  val ContractAccount = DataTypeVal(6,
    lenFixed          = true,
    maxLen            = vsys.account.ContractAccount.AddressLength,
    deserializer      = vsys.account.ContractAccount.fromBytes(_).right.get,
    serializer        = (a: vsys.account.ContractAccount) => a.bytes.arr,
    jsonifier         = (a: vsys.account.ContractAccount) => Json.toJson(a.address),
    extValidator      = vsys.account.ContractAccount.fromBytes(_).isRight)

  val Account         = DataTypeVal(7,
    lenFixed          = false,
    maxLen            = (vsys.account.Address.AddressLength).max(vsys.account.ContractAccount.AddressLength),
    deserializer      = vsys.account.Account.fromBytes(_, 0).right.get._1,
    serializer        = (a: vsys.account.Account) => a.bytes.arr,
    jsonifier         = (a: vsys.account.Account) => Json.toJson(a.bytes.arr),
    extValidator      = vsys.account.Account.fromBytes(_, 0).isRight)

  val TokenId         = DataTypeVal(8,
    lenFixed          = true,
    maxLen            = vsys.account.ContractAccount.TokenAddressLength,
    deserializer      = ByteStr(_),
    serializer        = (b: ByteStr) => b.arr,
    jsonifier         = (b: ByteStr) => Json.toJson(b.base58),
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

  val Timestamp       = DataTypeVal(9,
    lenFixed          = true,
    maxLen            = TimestampLength,
    deserializer      = Longs.fromByteArray(_),
    serializer        = Longs.toByteArray(_),
    jsonifier         = (t: Long) => Json.toJson(t),
    extValidator      = Longs.fromByteArray(_) >= 0)

  val Boolean         = DataTypeVal(10,
    lenFixed          = true,
    maxLen            = 1,
    deserializer      = b => b.head == 1,
    serializer        = (b: Boolean) => Array((if(b) 1 else 0).toByte),
    jsonifier         = (b: Boolean) => Json.toJson(b.toString),
    extValidator      = b => (b(0) == 1 || b(0) == 0))

  val ShortBytes      = DataTypeVal(11,
    lenFixed          = false,
    maxLen            = MaxShortBytesLength,
    deserializer      = b => b.drop(2),
    serializer        = (b: Array[Byte]) => arrayShortLengthToByteArray(b) ++ b,
    jsonifier         = (b: Array[Byte]) => Json.toJson(Base58.encode(b)),
    extValidator      = _ => true)

  val Balance         = DataTypeVal(12,
    lenFixed          = true,
    maxLen            = AmountLength,
    deserializer      = Longs.fromByteArray(_),
    serializer        = Longs.toByteArray(_),
    jsonifier         = (b: Long) => Json.toJson(b),
    extValidator      = Longs.fromByteArray(_) >= 0)

  val OpcBlock        = DataTypeVal(13,
    lenFixed          = false,
    maxLen            = MaxOpcBlockLength,
    deserializer      = b => b.drop(2),
    serializer        = (b: Array[Byte]) => arrayShortLengthToByteArray(b) ++ b,
    jsonifier         = (b: Array[Byte]) => Json.toJson(Base58.encode(b)),
    extValidator      = _ => true)

  val BigInteger      = DataTypeVal(14,
    lenFixed          = false,
    maxLen            = MaxBigIntLength,
    deserializer      = b => BigInt(b.drop(2)),
    serializer        = (i: BigInt) => arrayShortLengthToByteArray(i.toByteArray) ++ i.toByteArray,
    jsonifier         = (i: BigInt) => Json.toJson(i.toString),
    extValidator      = _ => true)

  def fromByte(b: Byte): Option[DataType.DataTypeVal[_]] = Try(DataType(b).asInstanceOf[DataTypeVal[_]]).toOption
  
  def check(a: Byte, b: Byte): Boolean = {
    if (a == b) true
    else if (a == DataType.Account.id) b == DataType.Address.id || b == DataType.ContractAccount.id
    else if (b == DataType.Account.id) check(b, a)
    else false
  }

  def checkTypes(paraTypes: Array[Byte], dataTypes: Array[Byte]): Boolean = {
    paraTypes.length == dataTypes.length && (paraTypes, dataTypes).zipped.forall { case (a, b) => check(a, b) }
  }

  def sqrt(y: BigInt): BigInt = {
    if (y > 3) Stream.iterate((y, (y >> 1) + 1)){ case (z, x) => (x, (y / x + x) >> 1) }.dropWhile{ case(z, x) => x < z }.head._1
    else 1
  }

  def arrayShortLengthToByteArray(a: Array[_]) = Shorts.toByteArray(a.length.toShort)

}
