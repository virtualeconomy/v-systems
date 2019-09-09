package vsys.blockchain.state.opcdiffs

import vsys.blockchain.transaction.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.transaction.{GenesisTransaction, ValidationError}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.ValidationError.ContractInvalidOPCData
import vsys.blockchain.contract._
import vsys.blockchain.transaction.contract._


class RegisterContractTransactionOpcDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with OpcFunction
  with TransactionGen
  with ContractGen
  with StateVar
  with TextualForm
  with DataStack {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  val languageCode: String = "vdds"
  val languageVersion: Int = 1

  val regWrongParaContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerWrongParaGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndRegContractWrongPara: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveLongGen
    contract1 <- regWrongParaContract
    data <- initDataStackGen(100000000L, 100L, "initializer")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create1: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, data, description, fee, feeScale, ts + 1).right.get
  } yield (genesis, create1, create1.transactionFee)

  property("register contract transaction cannot pass due to wrong list of parameters"){
    forAll(preconditionsAndRegContractWrongPara) { case (genesis, create, feeCreate) =>
      assertOpcFuncDifferEi(2, create) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe Left(ValidationError.ContractDataTypeMismatch)
      }
    }
  }

  val regContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndRegContract: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveLongGen
    contract1 <- regContract
    data <- initDataStackGen(100000000L, 100L, "initializer")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create1: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, data, description, fee, feeScale, ts + 1).right.get
  } yield (genesis, create1, create1.transactionFee)

  property("register contract transaction pass OpcFunDiff"){
    forAll(preconditionsAndRegContract) { case (genesis, create, _) =>
      assertOpcFuncDifferEi(2, create) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe an[Right[_, _]]
      }
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(create))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
      }
    }
  }

  val regWrongOpcFunContract: Gen[Contract] = contractNewGen(languageCode, languageVersion, triggerWrongTDBGen(), descriptorFullGen(), stateVarRightGen, textualRightGen)
  val preconditionsAndRegContractWrongFun: Gen[(GenesisTransaction, RegisterContractTransaction, Long)] = for {
    master <- accountGen
    ts <- positiveLongGen
    contract1 <- regWrongOpcFunContract
    data <- initDataStackGen(100000000L, 100L, "initializer")
    description <- validDescStringGen
    fee <- smallFeeGen
    feeScale <- feeScaleGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, -1, ts).right.get
    create1: RegisterContractTransaction = RegisterContractTransaction.create(master, contract1, data, description, fee, feeScale, ts + 1).right.get
  } yield (genesis, create1, create1.transactionFee)

  property("register contract transaction cannot pass due to wrong TDB opcode"){
    forAll(preconditionsAndRegContractWrongFun) { case (genesis, create, feeCreate) =>
      assertOpcFuncDifferEi(2, create) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe Left(ContractInvalidOPCData)
      }
    }
  }

}
