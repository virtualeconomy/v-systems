package vsys.transaction

import com.wavesplatform.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, TransactionParser}
import vsys.transaction.contract._
import com.wavesplatform.state2.diffs._
import scorex.transaction.TransactionParser.TransactionType
import vsys.contract._

class ExecuteContractTransactionTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with OpcFunction
  with TransactionGen
  with ContractGen
  with StateVar
  with Textual
  with DataStack {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val preconditionAndBuildExecuteContract: Gen[(Array[Byte], Array[Byte], Array[Byte], Seq[Array[Byte]], Seq[Array[Byte]], Seq[Array[Byte]])] = for {
    langCode <- languageCodeFromLengthGen(4)
    langVer <- languageVersionFromLengthGen(4)
    init <- initFunGen()
    descriptor <- descriptorFullGen()
    stateVar <- stateVarRandomGen()
    textual <- textualRandomGen()
  } yield (langCode, langVer, init, descriptor, stateVar, textual)

  property("execute contract build doesn't break invariant"){
    forAll(preconditionAndBuildExecuteContract) { case (langCode, langVer, init, descriptor, stateVar, textual) =>
      Contract.buildContract(langCode, langVer, init, descriptor, stateVar, textual) shouldBe an[Right[_, _]]
    }
  }

  val language: Seq[Int] = Seq(4, 4)
  val contractParse: Gen[Contract] = contractNewGen(language, initFunGen(), descriptorFullGen(), stateVarRightGen, textualRandomGen())
  val preconditionsAndParseExecuteContract: Gen[ExecuteContractFunctionTransaction] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- contractParse
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    timestamp2: Long <- positiveLongGen
    funcIdx: Short <- Gen.const(FunId.issue)
    data: Seq[DataEntry] <- dataEntryGen
  } yield ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx, data, description, fee2, feeScale2, timestamp2).right.get

  property("RegisterContractTransaction serialization roundtrip") {
    forAll(preconditionsAndParseExecuteContract) { tx: ExecuteContractFunctionTransaction =>
      require(tx.bytes.head == TransactionType.ExecuteContractFunctionTransaction.id)
      val recovered = ExecuteContractFunctionTransaction.parseTail(tx.bytes.tail).get
      assertTxs(recovered, tx)
    }
  }

  property("RegisterContractTransaction serialization from TypedTransaction") {
    forAll(preconditionsAndParseExecuteContract) { tx: ExecuteContractFunctionTransaction =>
      val recovered = TransactionParser.parseBytes(tx.bytes).get
      assertTxs(recovered.asInstanceOf[ExecuteContractFunctionTransaction], tx)
    }
  }

  private def assertTxs(first: ExecuteContractFunctionTransaction, second: ExecuteContractFunctionTransaction): Unit = {
    first.contractId.bytes.arr shouldEqual second.contractId.bytes.arr
    first.funcIdx shouldEqual second.funcIdx
    first.data.flatMap(_.bytes).toArray shouldEqual second.data.flatMap(_.bytes).toArray
    first.description shouldEqual second.description
    first.fee shouldEqual second.fee
    first.feeScale shouldEqual second.feeScale
    first.proofs.bytes shouldEqual second.proofs.bytes
    first.bytes shouldEqual second.bytes
  }

  val newContract: Gen[Contract] = contractNewGen(language, initFunGen(), descriptorFullGen(), stateVarRightGen, textualRandomGen())
  val preconditionsAndExecuteContractIssue: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveIntGen
    contract1 <- newContract
    dataStack: Seq[DataEntry] <- initDataStackGen(100000000L, 100L, "init")
    description <- genBoundedString(2, RegisterContractTransaction.MaxDescriptionSize)
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    regContract: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, dataStack, description, fee, feeScale, ts).right.get
    fee2: Long <- smallFeeGen
    feeScale2: Short <- feeScaleGen
    ts2: Long <- positiveLongGen
    funcIdx: Short <- Gen.const(FunId.issue)
    data: Seq[DataEntry] <- issueDataStackGen(10000L,0)
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    executeContract: ExecuteContractFunctionTransaction = ExecuteContractFunctionTransaction.create(master, regContract.contractId, funcIdx, data, description, fee2, feeScale2, ts2).right.get
  } yield (genesis, regContract, executeContract, executeContract.fee)

  property("execute contract transaction issue successfully"){
    forAll(preconditionsAndExecuteContractIssue) { case (genesis, regContract, executeContract, feeCreate) =>
//      assertOpcFuncDifferEi(2, create) { OpcFunDiffEi =>
//        OpcFunDiffEi shouldBe an[Right[_, _]]
//      }
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(regContract))), TestBlock.create(Seq(executeContract))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
      }
    }
  }
}
