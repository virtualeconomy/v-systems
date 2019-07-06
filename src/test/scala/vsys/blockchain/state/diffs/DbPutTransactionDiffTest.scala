package vsys.blockchain.state.diffs

import com.google.common.primitives.{Bytes, Longs, Shorts}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.PrivateKeyAccount
import vsys.blockchain.block.TestBlock
import vsys.blockchain.state.diffs.TransactionDiffer.TransactionValidationError
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, ValidationError}
import vsys.blockchain.transaction.database.DbPutTransaction
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.blockchain.transaction.ValidationError.InvalidDbKey
import vsys.blockchain.transaction.proof.{EllipticCurve25519Proof, Proofs}
import vsys.utils.serialization.{BytesSerializable, Deser}

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
    } yield (master, fee, feeScale, dbKey, entry, ts)) { case (master: PrivateKeyAccount, fee, feeScale, dbKey, entry, ts) =>
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
    } yield (master, fee, feeScale, entry, ts)) { case (master: PrivateKeyAccount, fee, feeScale, entry, ts) =>
      DbPutTransaction.create(master, "", entry, fee, feeScale, ts) shouldEqual Left(ValidationError.InvalidDbKey)
    }
  }

  property("disallows invalid utf8 db key") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      ts <- timestampGen
      dbKey <- invalidUtf8StringGen
      entry <- entryGen
      feeScale <- feeScaleGen
    } yield (master, fee, feeScale, dbKey, entry, ts)) { case (master: PrivateKeyAccount, fee, feeScale, dbKey, entry, ts) =>
      DbPutTransaction.create(master, dbKey, entry, fee, feeScale, ts) shouldEqual Left(ValidationError.InvalidUTF8String("dbKey"))
    }
  }

  property("disallows invalid utf8 db entry") {
    forAll(for {
      master <- accountGen
      fee <- smallFeeGen
      ts <- timestampGen
      dbKey <- validDbKeyStringGen
      dbData <- invalidUtf8StringGen
      feeScale <- feeScaleGen
    } yield (master, fee, feeScale, dbKey, dbData, ts)) { case (master: PrivateKeyAccount, fee, feeScale, dbKey, dbData, ts) =>
      DbPutTransaction.create(master, dbKey, "ByteArray", dbData, fee, feeScale, ts) shouldEqual Left(ValidationError.InvalidUTF8String("dbEntry"))
    }
  }

  /*
  This part test signed transaction cases. When a bad node broadcast an invalid dbKey length transaction to others,
  the invalid dbKey will be detected in DbPutTransactionDiff instead of transaction create
   */

  val preconditionsAndDbPut: Gen[(GenesisTransaction, DbPutTransaction)] = for {
    master <- accountGen
    ts <- positiveLongGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    ts2 <- positiveLongGen
    dbKey <- invalidLengthDbKeyStringGen
    entry <- entryGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen

    toSign: Array[Byte] = Bytes.concat(
      Array(TransactionType.DbPutTransaction.id.toByte),
      BytesSerializable.arrayWithSize(Deser.serilizeString(dbKey)),
      BytesSerializable.arrayWithSize(entry.bytes.arr),
      Longs.toByteArray(fee),
      Shorts.toByteArray(feeScale),
      Longs.toByteArray(ts2))

    proof = EllipticCurve25519Proof.createProof(toSign, master)
    proofs: Proofs = Proofs.create(List(proof.bytes)).right.get
    dbPut: DbPutTransaction = DbPutTransaction(dbKey, entry, fee, feeScale, ts2, proofs)
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
