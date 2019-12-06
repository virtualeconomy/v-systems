package vsys.blockchain.state.contract.token

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.state.EitherExt2
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData}
import vsys.blockchain.contract._
import vsys.blockchain.contract.token.{TokenContractGen, TokenFunction}
import vsys.blockchain.transaction.contract._


class TokenContractOpcDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with TokenFunction {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val regContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractPermitted.contract.languageCode, ContractPermitted.contract.languageVersion,
      ContractPermitted.contract.trigger, ContractPermitted.contractWithoutSplit.descriptor, ContractPermitted.contract.stateVar,
      ContractPermitted.contract.stateMap, ContractPermitted.contractWithoutSplit.textual).explicitGet())
  val preconditionsAndRegContract: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regContract
    data <- initTokenDataStackGen(100000000L, 100L, "initializer")
    description <- validDescStringGen
    genesis <- genesisTokenGen(master, ts)
    create <- registerTokenGen(master, contract, data, description, fee, ts + 1)
  } yield (genesis, create)

  property("register contract transaction pass OpcFunDiff"){
    forAll(preconditionsAndRegContract) { case (genesis, create) =>
      assertOpcFuncDifferEi(2, None, create) { opcFunDiffEi =>
        opcFunDiffEi shouldBe an[Right[_, _]]
      }
      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.createWithTxStatus(Seq(create), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
      }
    }
  }

  val regWrongParaContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractPermitted.contract.languageCode, ContractPermitted.contract.languageVersion,
      wrongDataTrigger, ContractPermitted.contract.descriptor, ContractPermitted.contract.stateVar,
      ContractPermitted.contract.stateMap, ContractPermitted.contract.textual).explicitGet())
  val preconditionsAndRegContractWrongPara: Gen[RegisterContractTransaction] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regWrongParaContract
    data <- initTokenDataStackGen(100000000L, 100L, "initializer")
    description <- validDescStringGen
    create <- registerTokenGen(master, contract, data, description, fee, ts)
  } yield create

  property("token contract transaction cannot pass due to wrong list of parameters"){
    forAll(preconditionsAndRegContractWrongPara) { case create =>
      assertOpcFuncDifferEi(2, None, create) { opcFunDiffEi =>
        opcFunDiffEi shouldBe Left(ContractDataTypeMismatch)
      }
    }
  }

  val regWrongOpcFunContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractPermitted.contract.languageCode, ContractPermitted.contract.languageVersion,
      wrongOpcTrigger, ContractPermitted.contract.descriptor, ContractPermitted.contract.stateVar,
      ContractPermitted.contract.stateMap, ContractPermitted.contract.textual).explicitGet())
  val preconditionsAndRegContractWrongFun: Gen[RegisterContractTransaction] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regWrongOpcFunContract
    data <- initTokenDataStackGen(100000000L, 100L, "initializer")
    description <- validDescStringGen
    create <- registerTokenGen(master, contract, data, description, fee, ts)
  } yield create

  property("register contract transaction cannot pass due to wrong opcode"){
    forAll(preconditionsAndRegContractWrongFun) { case create =>
      assertOpcFuncDifferEi(2, None, create) { opcFunDiffEi =>
        opcFunDiffEi shouldBe Left(ContractInvalidOPCData)
      }
    }
  }

}
