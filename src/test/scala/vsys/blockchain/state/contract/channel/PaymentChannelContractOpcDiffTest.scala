package vsys.blockchain.state.contract.channel

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
import vsys.blockchain.contract.channel.{PaymentChannelContractGen, PaymentChannelFunction}
import vsys.blockchain.transaction.contract._


class PaymentChannelContractOpcDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with PaymentChannelContractGen
  with PaymentChannelFunction {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val regContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractPaymentChannel.contract.languageCode, ContractPaymentChannel.contract.languageVersion,
      ContractPaymentChannel.contract.trigger, ContractPaymentChannel.contract.descriptor, ContractPaymentChannel.contract.stateVar,
      ContractPaymentChannel.contract.stateMap, ContractPaymentChannel.contract.textual).explicitGet())

  val preconditionsAndRegContract: Gen[(GenesisTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regContract
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    description <- validDescStringGen
    genesis <- genesisPaymentChannelGen(master, ts)
    create <- registerPaymentChannelGen(master, contract, data, description, fee, ts + 1)
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
    Gen.const(Contract.buildContract(ContractPaymentChannel.contract.languageCode, ContractPaymentChannel.contract.languageVersion,
      wrongDataTrigger, ContractPaymentChannel.contract.descriptor, ContractPaymentChannel.contract.stateVar,
      ContractPaymentChannel.contract.stateMap, ContractPaymentChannel.contract.textual).explicitGet())
  val preconditionsAndRegContractWrongPara: Gen[RegisterContractTransaction] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regWrongParaContract
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    description <- validDescStringGen
    create <- registerPaymentChannelGen(master, contract, data, description, fee, ts)
  } yield create

  property("payment channel contract transaction cannot pass due to wrong list of parameters"){
    forAll(preconditionsAndRegContractWrongPara) { case create =>
      assertOpcFuncDifferEi(2, None, create) { opcFunDiffEi =>
        opcFunDiffEi shouldBe Left(ContractDataTypeMismatch)
      }
    }
  }

  val regWrongOpcFunContract: Gen[Contract] =
    Gen.const(Contract.buildContract(ContractPaymentChannel.contract.languageCode, ContractPaymentChannel.contract.languageVersion,
      wrongOpcTrigger, ContractPaymentChannel.contract.descriptor, ContractPaymentChannel.contract.stateVar,
      ContractPaymentChannel.contract.stateMap, ContractPaymentChannel.contract.textual).explicitGet())
  val preconditionsAndRegContractWrongFun: Gen[RegisterContractTransaction] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    contract <- regWrongOpcFunContract
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    description <- validDescStringGen
    create <- registerPaymentChannelGen(master, contract, data, description, fee, ts)
  } yield create

  property("register contract transaction cannot pass due to wrong opcode"){
    forAll(preconditionsAndRegContractWrongFun) { case create =>
      assertOpcFuncDifferEi(2, None, create) { opcFunDiffEi =>
        opcFunDiffEi shouldBe Left(ContractInvalidOPCData)
      }
    }
  }
}
