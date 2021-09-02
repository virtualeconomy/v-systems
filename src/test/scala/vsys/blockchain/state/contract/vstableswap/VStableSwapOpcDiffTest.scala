package vsys.blockchain.state.contract.vstableswap

import com.google.common.primitives.Ints
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.state.EitherExt2
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData}
import vsys.blockchain.contract._
import vsys.blockchain.contract.vstableswap.{VStableSwapContractGen, VStableSwapFunction}
import vsys.blockchain.transaction.contract._

class VStableSwapOpcDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with VStableSwapContractGen
  with VStableSwapFunction  {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val regContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractVStableSwap.contract.languageCode, ContractVStableSwap.contract.languageVersion,
      ContractVStableSwap.contract.trigger, ContractVStableSwap.contract.descriptor, ContractVStableSwap.contract.stateVar,
      ContractVStableSwap.contract.stateMap, ContractVStableSwap.contract.textual).explicitGet())

  val preconditionsAndRegContract: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regContract
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data <- initVStableSwapDataStackGen(tokenId.arr, tokenId.arr, 5, 1, 1)
    description <- validDescStringGen
    genesis <- genesisVStableSwapGen(master, ts)
    create <- registerVStableSwapGen(master, contract, data, description, fee, ts + 1)
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
    Gen.const(Contract.buildContract(ContractVStableSwap.contract.languageCode, ContractVStableSwap.contract.languageVersion,
      wrongDataTrigger, ContractVStableSwap.contract.descriptor, ContractVStableSwap.contract.stateVar,
      ContractVStableSwap.contract.stateMap, ContractVStableSwap.contract.textual).explicitGet())
  val preconditionsAndRegContractWrongPara: Gen[RegisterContractTransaction] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regWrongParaContract
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data <- initVStableSwapDataStackGen(tokenId.arr, tokenId.arr, 5, 1, 1)
    description <- validDescStringGen
    create <- registerVStableSwapGen(master, contract, data, description, fee, ts)
  } yield create

  property("register contract transaction cannot pass due to wrong list of parameters"){
    forAll(preconditionsAndRegContractWrongPara) { case create =>
      assertOpcFuncDifferEi(2, None, create) { opcFunDiffEi =>
        opcFunDiffEi shouldBe Left(ContractDataTypeMismatch)
      }
    }
  }

  val regWrongOpcFunContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractVStableSwap.contract.languageCode, ContractVStableSwap.contract.languageVersion,
      wrongOpcTrigger, ContractVStableSwap.contract.descriptor, ContractVStableSwap.contract.stateVar,
      ContractVStableSwap.contract.stateMap, ContractVStableSwap.contract.textual).explicitGet())
  val preconditionsAndRegContractWrongFun: Gen[RegisterContractTransaction] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regWrongOpcFunContract
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data <- initVStableSwapDataStackGen(tokenId.arr, tokenId.arr, 5, 1, 1)
    description <- validDescStringGen
    create <- registerVStableSwapGen(master, contract, data, description, fee, ts)
  } yield create

  property("register contract transaction cannot pass due to wrong opcode"){
    forAll(preconditionsAndRegContractWrongFun) { case create =>
      assertOpcFuncDifferEi(2, None, create) { opcFunDiffEi =>
        opcFunDiffEi shouldBe Left(ContractInvalidOPCData)
      }
    }
  }
}
