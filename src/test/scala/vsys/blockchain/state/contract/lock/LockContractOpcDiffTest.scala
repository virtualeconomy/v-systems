package vsys.blockchain.state.contract.lock

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
import vsys.blockchain.contract.lock.{LockContractGen, LockFunction}
import vsys.blockchain.transaction.contract._


class LockContractOpcDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with LockContractGen
  with LockFunction {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val regContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractLock.contract.languageCode, ContractLock.contract.languageVersion,
      ContractLock.contract.trigger, ContractLock.contract.descriptor, ContractLock.contract.stateVar,
      ContractLock.contract.stateMap, ContractLock.contract.textual).explicitGet())
  val preconditionsAndLockContract: Gen[(GenesisTransaction, RegisterContractTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regContract
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data <- initLockContractDataStackGen(sysTokenId.arr)
    description <- validDescStringGen
    genesis <- genesisLockGen(master, ts)
    regContract <- registerLockContractGen(master, contract, data, description, fee + 10000000000L, ts + 1)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    lockV <- lockAssetGen(master, contractId, ts + 2, attach, fee, ts)
  } yield (genesis, regContract, lockV)

  property("execute lock asset contract transaction pass OpcFunDiff"){
    forAll(preconditionsAndLockContract) { case (genesis, create, lockV) =>
      assertOpcFuncDifferEi(2, Option(create), lockV) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe an[Right[_, _]]
      }
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, create))), TestBlock.createWithTxStatus(Seq(lockV), TransactionStatus.Success)) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
      }
    }
  }

  val regWrongParaContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractLock.contract.languageCode, ContractLock.contract.languageVersion,
      ContractLock.contract.trigger, wrongDataFunc, ContractLock.contract.stateVar,
      ContractLock.contract.stateMap, ContractLock.contract.textual).explicitGet())

  val regWrongOpcFunContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractLock.contract.languageCode, ContractLock.contract.languageVersion,
      ContractLock.contract.trigger, wrongOpcFunc, ContractLock.contract.stateVar,
      ContractLock.contract.stateMap, ContractLock.contract.textual).explicitGet())

  val preconditionsAndLockContractInvalid: Gen[(RegisterContractTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract1 <- regWrongParaContract
    contract2 <- regWrongOpcFunContract
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data <- initLockContractDataStackGen(sysTokenId.arr)
    description <- validDescStringGen
    regContract1 <- registerLockContractGen(master, contract1, data, description, fee + 10000000000L, ts + 1)
    contractId1 = regContract1.contractId
    regContract2 <- registerLockContractGen(master, contract2, data, description, fee + 10000000000L, ts + 1)
    contractId2 = regContract2.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    lockV1 <- lockAssetGen(master, contractId1, ts + 2, attach, fee, ts)
    lockV2 <- lockAssetGen(master, contractId2, ts + 2, attach, fee, ts)
  } yield (regContract1, regContract2, lockV1, lockV2)

  property("lock asset contract transaction cannot pass due to wrong list of parameters"){
    forAll(preconditionsAndLockContractInvalid) { case (reg1, _, lockWrongPara, _) =>
      assertOpcFuncDifferEi(2, Option(reg1), lockWrongPara) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe Left(ContractDataTypeMismatch)
      }
    }
  }

  property("lock asset contract transaction cannot pass due to wrong opcode"){
    forAll(preconditionsAndLockContractInvalid) { case (_, reg2, _, lockWrongOpcFunc) =>
      assertOpcFuncDifferEi(2, Option(reg2), lockWrongOpcFunc) { OpcFunDiffEi =>
        OpcFunDiffEi shouldBe Left(ContractInvalidOPCData)
      }
    }
  }

}
