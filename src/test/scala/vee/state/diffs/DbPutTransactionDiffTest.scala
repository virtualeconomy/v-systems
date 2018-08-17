package vee.state.diffs

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.{GenesisTransaction, ValidationError}
import vee.transaction.database.DbPutTransaction
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.diffs.{ENOUGH_AMT, assertDiffEi}
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import scorex.crypto.EllipticCurveImpl
import scorex.lagonaki.mocks.TestBlock
import scorex.serialization.BytesSerializable
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.InvalidDbKey

class DbPutTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  property("disallows db key larger than max key length") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      ts <- timestampGen
      dbKey <- invalidLengthDbKeyStringGen
      entry <- entryGen
      feeScale <- feeScaleGen
    } yield (master, fee, feeScale, dbKey, entry, ts)) { case (master, fee, feeScale, dbKey, entry, ts) =>
      DbPutTransaction.create(master, dbKey, entry, fee, feeScale, ts) shouldEqual Left(ValidationError.InvalidDbKey)
    }
  }

  property("disallows empty db key length") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      ts <- timestampGen
      entry <- entryGen
      feeScale <- feeScaleGen
    } yield (master, fee, feeScale, entry, ts)) { case (master, fee, feeScale, entry, ts) =>
      DbPutTransaction.create(master, "", entry, fee, feeScale, ts) shouldEqual Left(ValidationError.InvalidDbKey)
    }
  }


  /*
  This part test signed transaction cases. When a bad node broadcast an invalid dbKey length transaction to others,
  the invalid dbKey will be detected in DbPutTransactionDiff instead of transaction create
   */

  val preconditionsAndDbPut: Gen[(GenesisTransaction, DbPutTransaction)] = for {
    master <- accountGen
    ts <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    ts2 <- positiveLongGen
    dbKey <- invalidLengthDbKeyStringGen
    entry <- entryGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen

    toSign: Array[Byte] = Bytes.concat(
      Array(TransactionType.DbPutTransaction.id.toByte),
      master.publicKey,
      BytesSerializable.arrayWithSize(dbKey.getBytes("UTF-8")),
      BytesSerializable.arrayWithSize(entry.bytes.arr),
      Longs.toByteArray(fee),
      Shorts.toByteArray(feeScale),
      Longs.toByteArray(ts2))

    dbPut: DbPutTransaction = DbPutTransaction(master, dbKey, entry, fee, feeScale, ts2, ByteStr(EllipticCurveImpl.sign(master, toSign)))
  } yield (genesis, dbPut)

  property("disallows invalid dbKey length in Signed db put Transaction") {
    forAll(preconditionsAndDbPut) {
      case (genesis, dbPut) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(dbPut))) { totalDiffEi =>
          totalDiffEi shouldBe Left(TransactionValidationError(InvalidDbKey, dbPut))
        }
    }
  }

}
