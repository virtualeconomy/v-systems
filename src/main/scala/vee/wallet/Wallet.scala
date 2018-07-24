package vee.wallet

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import scorex.crypto.hash.SecureCryptographicHash
import com.wavesplatform.settings.WalletSettings
import com.wavesplatform.state2.ByteStr
import vee.utils.JsonFileStorage
import play.api.libs.json._
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.MissingSenderPrivateKey
import scorex.utils.{ScorexLogging, randomString}
import scorex.crypto.encode.Base58

import scala.collection.concurrent.TrieMap
import scala.util.control.NonFatal

trait Wallet {

  def seed: Array[Byte]

  def nonce: Int

  def privateKeyAccounts: List[PrivateKeyAccount]

  def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount]

  def generateNewAccount(): Option[PrivateKeyAccount]

  def deleteAccount(account: PrivateKeyAccount): Boolean

  def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount]

}

object Wallet extends ScorexLogging {

  implicit class WalletExtension(w: Wallet) {
    def findPrivateKey(addressString: String): Either[ValidationError, PrivateKeyAccount] =
      for {
        acc        <- Address.fromString(addressString)
        privKeyAcc <- w.privateKeyAccount(acc)
      } yield privKeyAcc

    def exportAccountSeed(account: Address): Either[ValidationError, Array[Byte]] = w.privateKeyAccount(account).map(_.seed)
  }

  private case class WalletData(seed: String, accountSeeds: Set[ByteStr], nonce: Int)

  private implicit val walletFormat: Format[WalletData] = Json.format

  def generateNewAccount(seed: Array[Byte], nonce: Int): PrivateKeyAccount = {
    val accountSeed = generateAccountSeed(seed, nonce)
    PrivateKeyAccount(accountSeed)
  }

  def generateAccountSeed(seed: Array[Byte], nonce: Int): Array[Byte] =
    SecureCryptographicHash(Bytes.concat(Ints.toByteArray(nonce), seed))

  def apply(settings: WalletSettings): Wallet = new WalletImpl(settings.file, settings.password, settings.seed)

  def convertWallet(oldPath: String, newPath: String): Unit = {
    val oldFilePath = new File(oldPath)
    val oldWallet = new WalletObsolete(Option(oldFilePath), "".toCharArray, None)

    val oldSeed = new String(Base58.decode(oldWallet.seed), StandardCharsets.UTF_8)
    val oldAccountSeeds = oldWallet.privateKeyAccounts().map(a => ByteStr(a.seed)).toSet
    val oldNonce = oldWallet.nonce()
    oldWallet.close()

    val WD = WalletData(oldSeed,oldAccountSeeds,oldNonce)

    JsonFileStorage.save(WD, newPath, Option(JsonFileStorage.prepareKey("")))
  }

  private class WalletImpl(maybeFile: Option[File], password: String, maybeSeedFromConfig: Option[String]) extends ScorexLogging with Wallet {

    private val key = JsonFileStorage.prepareKey(password)

    private def loadOrImport(f: File): Option[WalletData] =
      try {
        Some(JsonFileStorage.load[WalletData](f.getCanonicalPath, Some(key)))
      } catch {
        case NonFatal(_) => None
      }

    private lazy val actualSeed = maybeSeedFromConfig.getOrElse {
      val randomSeed = ByteStr(randomBytes(64)).toString
      log.info(s"Your randomly generated seed is ${randomSeed}")
      randomSeed
    }

    private var walletData: WalletData = {
      if (maybeFile.isEmpty)
        WalletData(actualSeed, Set.empty, 0)
      else {
        val file = maybeFile.get
        if (file.exists() && file.length() > 0) {
          val wd = loadOrImport(maybeFile.get)
          if (wd.isDefined) wd.get
          else {
            throw new IllegalStateException(s"Failed to open existing wallet file '${maybeFile.get}' maybe provided password is incorrect")
          }
        } else WalletData(actualSeed, Set.empty, 0)
      }
    }

    private val l = new Object

    private def lock[T](f: => T): T = l.synchronized(f)

    private val accountsCache: TrieMap[String, PrivateKeyAccount] = {
      val accounts = walletData.accountSeeds.map(seed => PrivateKeyAccount(seed.arr))
      TrieMap(accounts.map(acc => acc.address -> acc).toSeq: _*)
    }

    private def save(): Unit = maybeFile.foreach(f => JsonFileStorage.save(walletData, f.getCanonicalPath, Some(key)))

    private def generateNewAccountWithoutSave(): Option[PrivateKeyAccount] = lock {
      val nonce   = getAndIncrementNonce()
      val account = Wallet.generateNewAccount(seed, nonce)

      val address = account.address
      if (!accountsCache.contains(address)) {
        accountsCache += account.address -> account
        walletData = walletData.copy(accountSeeds = walletData.accountSeeds + ByteStr(account.seed))
        log.info("Added account #" + privateKeyAccounts.size)
        Some(account)
      } else None
    }

    override def seed: Array[Byte] = walletData.seed.arr

    override def privateKeyAccounts: List[PrivateKeyAccount] = accountsCache.values.toList

    override def generateNewAccounts(howMany: Int): Seq[PrivateKeyAccount] =
      (1 to howMany).flatMap(_ => generateNewAccount())

    override def generateNewAccount(): Option[PrivateKeyAccount] = lock {
      generateNewAccountWithoutSave().map(acc => {
        save()
        acc
      })
    }

    override def deleteAccount(account: PrivateKeyAccount): Boolean = lock {
      val before = walletData.accountSeeds.size
      walletData = walletData.copy(accountSeeds = walletData.accountSeeds - ByteStr(account.seed))
      accountsCache -= account.address
      save()
      before > walletData.accountSeeds.size
    }

    override def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount] =
      accountsCache.get(account.address).toRight[ValidationError](MissingSenderPrivateKey)

    override def nonce: Int = walletData.nonce

    private def getAndIncrementNonce(): Int = lock {
      val r = walletData.nonce
      walletData = walletData.copy(nonce = walletData.nonce + 1)
      r
    }

  }

}
