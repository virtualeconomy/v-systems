package vsys.transaction.contract

import com.wavesplatform.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, TransactionParser, ValidationError}
import com.wavesplatform.state2.diffs._
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.GenericError
import vsys.contract._


class RegContractTransactionTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with OpcFunction
  with TransactionGen
  with ContractGen
  with StateVar
  with Texture
  with DataStack {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionAndBuildRegContract: Gen[(Array[Byte], Array[Byte], Array[Byte], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- languageCodeFromLengthGen(4)
    langVer <- languageVersionFromLengthGen(4)
    init <- initFunGen()
    descriptor <- descriptorFullGen()
    stateVar <- stateVarRandomGen()
    textual <- textureRightGen
  } yield (langCode, langVer, init, descriptor, stateVar, textual)

  property("register contract build doesn't break invariant"){
    forAll(preconditionAndBuildRegContract) { case (langCode, langVer, init, descriptor, stateVar, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, textual) shouldBe an[Right[_, _]]
    }
  }

  val languageCode: Int = 0
  val languageVersion: Int = 0
  val regContractParse: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndParseRegContract: Gen[RegisterContractTransaction] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- regContractParse
    data: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    create1: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, data, description, fee, feeScale, ts).right.get
  } yield create1

  property("RegisterContractTransaction serialization roundtrip") {
    forAll(preconditionsAndParseRegContract) { tx: RegisterContractTransaction =>
      require(tx.bytes.head == TransactionType.RegisterContractTransaction.id)
      val recovered = RegisterContractTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("RegisterContractTransaction serialization from TypedTransaction") {
    forAll(preconditionsAndParseRegContract) { tx: RegisterContractTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[RegisterContractTransaction], tx)
    }
  }

  private def assertTxs(first: RegisterContractTransaction, second: RegisterContractTransaction): Unit = {
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.contractId.bytes.arr shouldEqual second.contractId.bytes.arr
    first.contract.descriptor.toArray shouldEqual second.contract.descriptor.toArray
    first.contract.initializer shouldEqual second.contract.initializer
    first.contract.languageCode shouldEqual second.contract.languageCode
    first.contract.languageVersion shouldEqual second.contract.languageVersion
    first.data.flatMap(_.bytes).toArray shouldEqual second.data.flatMap(_.bytes).toArray
    first.description shouldEqual second.description
  }

  val regWrongParaContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunWrongParaGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndRegContractWrongPara: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- regWrongParaContract
    data <- initDataStackGen(100000000L, 100L, "initializer")
    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create1: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, data, description, fee, feeScale, ts + 1).right.get
  } yield (genesis, create1, create1.fee)

  property("register contract transaction cannot pass due to wrong list of parameters"){
    forAll(preconditionsAndRegContractWrongPara) { case (genesis, create, feeCreate) =>
      assertOpcFuncDifferEi(2, create) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe Left(ValidationError.InvalidDataEntry)
      }
    }
  }

  val regContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, initFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndRegContract: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- regContract
    data <- initDataStackGen(100000000L, 100L, "initializer")
    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create1: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, data, description, fee, feeScale, ts + 1).right.get
  } yield (genesis, create1, create1.fee)

  property("register contract transaction pass OpcFunDiff"){
    forAll(preconditionsAndRegContract) { case (genesis, create, feeCreate) =>
      assertOpcFuncDifferEi(2, create) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe an[Right[_, _]]
      }
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(create))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
      }
    }
  }

  val regWrongOpcFunContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, initWrongTDBFunGen(), descriptorFullGen(), stateVarRightGen, textureRightGen)
  val preconditionsAndRegContractWrongFun: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- regWrongOpcFunContract
    data <- initDataStackGen(100000000L, 100L, "initializer")
    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create1: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, data, description, fee, feeScale, ts + 1).right.get
  } yield (genesis, create1, create1.fee)

  property("register contract transaction cannot pass due to wrong TDB opcode"){
    forAll(preconditionsAndRegContractWrongFun) { case (genesis, create, feeCreate) =>
      assertOpcFuncDifferEi(2, create) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe Left(GenericError("Wrong TDB opcode"))
      }
    }
  }

}
