package vee.transaction.database

import com.wavesplatform.TransactionGen
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction._
import vee.database.Entry

class DbPutTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  val dbPutTxWithInvalidFeeScale: Gen[(Either[_, _], Short)] = for {
    timestamp: Long <- positiveLongGen
    sender: PrivateKeyAccount <- accountGen
    name: String <- validAliasStringGen
    entry: Entry <- entryGen
    fee: Long <- smallFeeGen
    feeScale: Short <- Gen.choose[Short](1,99)
  } yield (DbPutTransaction.create(sender, name, entry, fee, feeScale, timestamp), feeScale)

  property("DbPutTransaction serialization roundtrip") {
    forAll(dbPutGen) { tx: DbPutTransaction =>
      require(tx.bytes.head == TransactionType.DbPutTransaction.id)
      val recovered = DbPutTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("DbPutTransaction serialization from TypedTransaction") {
    forAll(dbPutGen) { tx: DbPutTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[DbPutTransaction], tx)
    }
  }

  property("Invalid Fee Scale") {
    forAll(dbPutTxWithInvalidFeeScale) { case ((tx, feeScale)) =>
      tx shouldBe Left(ValidationError.WrongFeeScale(feeScale))
    }
  }

  private def assertTxs(first: DbPutTransaction, second: DbPutTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.entry.data shouldEqual second.entry.data
    first.entry.dataType shouldEqual second.entry.dataType
    first.name shouldEqual second.name
    first.bytes shouldEqual second.bytes
  }
}
