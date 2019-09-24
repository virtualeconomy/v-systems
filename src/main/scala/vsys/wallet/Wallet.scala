package vsys.wallet

import java.io.File

import vsys.utils.crypto.hash.SecureCryptographicHash
import vsys.settings.WalletSettings
import vsys.blockchain.state.ByteStr
import vsys.Version
import play.api.libs.json._
import vsys.account.{Address, PrivateKeyAccount, AddressScheme}
import vsys.blockchain.transaction.ValidationError
import vsys.blockchain.transaction.ValidationError.MissingSenderPrivateKey
import vsys.utils.{JsonFileStorage, ScorexLogging, randomBytes}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.LinkedHashSet
import scala.util.{DynamicVariable, Failure, Success, Try}

trait Wallet {

  def seed: String

  def nonce: Long

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

  private val chainName = if(AddressScheme.current.value.chainId == 'T') "testnet" else "mainnet"
  private val agentString = s"V Systems Wallet Specification:1.0/V Core:${Version.VersionString}/${chainName}"

  private case class WalletData(seed: String, accountSeeds: LinkedHashSet[ByteStr] = LinkedHashSet.empty, nonce: Long = 0, agent: String = agentString)

  private implicit val walletFormat: Format[WalletData] = Json.format

  def generateNewAccount(seed: String, nonce: Long): PrivateKeyAccount = {
    val accountSeed = generateAccountSeed(seed, nonce)
    PrivateKeyAccount(accountSeed)
  }

  def generateAccountSeed(seed: String, nonce: Long): Array[Byte] = {
    SecureCryptographicHash((nonce.toString + seed).getBytes("UTF-8"))
  }

  def apply(settings: WalletSettings): Wallet = new WalletImpl(settings.file, settings.password, settings.seed)

  private class WalletImpl(maybeFile: Option[File], password: String, maybeSeedFromConfig: Option[String]) extends ScorexLogging with Wallet {

    private val key = JsonFileStorage.prepareKey(password)

    private def loadOrImport(f: File): Option[WalletData] =
      Try {
        Some(JsonFileStorage.load[WalletData](f.getCanonicalPath, Some(key)))
      } match {
        case Success(w) => w
        case Failure(t: Throwable) => {
          log.error("Invalid Wallet error", t)
          None
        }
      }

    private lazy val actualSeed = maybeSeedFromConfig.getOrElse {
      val randomSeed = ByteStr(randomBytes(64)).toString
      log.info(s"Your randomly generated seed is ${randomSeed}")
      randomSeed
    }

    private val walletData: DynamicVariable[WalletData] = new DynamicVariable({
      if (maybeFile.isEmpty)
        WalletData(seed = actualSeed)
      else {
        val file = maybeFile.get
        if (file.exists() && file.length() > 0) {
          val wd = loadOrImport(maybeFile.get)
          if (wd.isDefined) wd.get
          else {
            throw new IllegalStateException(s"Failed to open existing wallet file '${maybeFile.get}' maybe provided password is incorrect")
          }
        } else WalletData(seed = actualSeed)
      }
    })

    private val l = new Object

    private def lock[T](f: => T): T = l.synchronized(f)

    private val accountsCache: TrieMap[String, PrivateKeyAccount] = {
      val accounts = walletData.value.accountSeeds.map(seed => PrivateKeyAccount(seed.arr))
      TrieMap(accounts.map(acc => acc.address -> acc).toSeq: _*)
    }

    private def save(): Unit = maybeFile.foreach(f => JsonFileStorage.save(walletData.value, f.getCanonicalPath, Some(key)))

    private def generateNewAccountWithoutSave(): Option[PrivateKeyAccount] = lock {
      val nonce   = getAndIncrementNonce()
      val account = Wallet.generateNewAccount(seed, nonce)

      val address = account.address
      if (!accountsCache.contains(address)) {
        accountsCache += account.address -> account
        walletData.value_=(walletData.value.copy(accountSeeds = walletData.value.accountSeeds + ByteStr(account.seed)))
        log.info("Added account #" + privateKeyAccounts.size)
        Some(account)
      } else None
    }

    override def seed: String = walletData.value.seed

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
      val before = walletData.value.accountSeeds.size
      walletData.value_=(walletData.value.copy(accountSeeds = walletData.value.accountSeeds - ByteStr(account.seed)))
      accountsCache -= account.address
      save()
      before > walletData.value.accountSeeds.size
    }

    override def privateKeyAccount(account: Address): Either[ValidationError, PrivateKeyAccount] =
      accountsCache.get(account.address).toRight[ValidationError](MissingSenderPrivateKey)

    override def nonce: Long = walletData.value.nonce

    private def getAndIncrementNonce(): Long = lock {
      val r = walletData.value.nonce
      walletData.value_=(walletData.value.copy(nonce = walletData.value.nonce + 1))
      r
    }

  }

}
