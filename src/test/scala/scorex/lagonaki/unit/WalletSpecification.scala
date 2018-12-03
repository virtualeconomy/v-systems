package scorex.lagonaki.unit

import com.wavesplatform.settings.WalletSettings
import org.scalatest.{FunSuite, Matchers}
import vsys.wallet.Wallet

class WalletSpecification extends FunSuite with Matchers {

  private val walletSize = 10
  val w = Wallet(WalletSettings(None, "cookies", Option("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz")))

  test("wallet - acc creation") {
    w.generateNewAccounts(walletSize)

    w.privateKeyAccounts.size shouldBe walletSize
    w.privateKeyAccounts.map(_.address) shouldBe Seq("AU1LeG5cWjFz6jRtZHUkGGENr9fBAQgTUen", "ATzbvcgTUc57YCeHZvcGJGvPXRxHt42w9Tt", "AU2qzimWpe7e78vksS4ufZ8exGzY6o7fYZJ", "ATxt5tn81hXmEaCzuGB7WbJjNfrtRfsAUyt", "AU3AorwRqQhYpRUR3ednaWFvuxAjYCNWd26", "AUCMSjySunvAHLfBMRDeKJZJSaq2zrEq6UH", "AU31hWDRTGCfHuSUxMfnzxJrzdQRdFgTPKM", "AU8oJ1QhnqcNo7qUjaWi1qHcbuEJEDXWSe5", "ATuYeyAT3HBkMQkbZRoyjR75Ajxd1ppWBYV", "AU6vdNHSWJtYCvqj1hNNb4HQxwdkKGCJ55R")
  }

  test("wallet - acc deletion") {

    val head = w.privateKeyAccounts.head
    w.deleteAccount(head)
    assert(w.privateKeyAccounts.size == walletSize - 1)

    w.deleteAccount(w.privateKeyAccounts.head)
    assert(w.privateKeyAccounts.size == walletSize - 2)

    w.privateKeyAccounts.foreach(w.deleteAccount)

    assert(w.privateKeyAccounts.isEmpty)
  }

  test("reopening") {

    val walletFile = Some(scorex.createTestTemporaryFile("wallet", ".dat"))

    val w = Wallet(WalletSettings(walletFile, "cookies", Option("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz")))
    w.generateNewAccounts(10)
    val nonce = w.nonce

    val w2 = Wallet(WalletSettings(walletFile, "cookies", None))
    w2.privateKeyAccounts.head.address should not be null
    w2.nonce shouldBe nonce
  }
}