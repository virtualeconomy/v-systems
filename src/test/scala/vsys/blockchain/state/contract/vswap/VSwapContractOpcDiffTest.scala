package vsys.blockchain.state.contract.vswap

import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{Contract, ContractGenHelper, DataEntry}
import vsys.blockchain.contract.ContractVSwap._
import vsys.utils.serialization.Deser
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.blockchain.contract.vswap.{VSwapContractGen, VSwapFunction}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.TransactionGen

class VSwapContractOpcDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with VSwapContractGen
  with VSwapFunction {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val regWrongOpcFunContract: Gen[Contract] =
    Gen.const(Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
      wrongOpcTrigger,
      Seq(supersedeFunc, setSwapFunc, addLiquidityFunc, removeLiquidityFunc, swapTokenForExactBaseTokenFunc,
        swapExactTokenForBaseTokenFunc, swapTokenForExactTargetTokenFunc, swapExactTokenForTargetTokenFunc),
        stateVarSeq, stateMapSeq, Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)).explicitGet())

  val regWrongDataContract: Gen[Contract] =
    Gen.const(Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
      wrongDataTrigger,
      Seq(supersedeFunc, setSwapFunc, addLiquidityFunc, removeLiquidityFunc, swapTokenForExactBaseTokenFunc,
        swapExactTokenForBaseTokenFunc, swapTokenForExactTargetTokenFunc, swapExactTokenForTargetTokenFunc),
        stateVarSeq, stateMapSeq, Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)).explicitGet())

  val validContract: Gen[Contract] = vSwapContractGen()
  val preconditionsAndRegContractWrongFun: Gen[(RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    validContract <- validContract
    wrongOpcContract <- regWrongOpcFunContract
    wrongDataContract <- regWrongDataContract

    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    data: Seq[DataEntry] <- initVSwapDataStackGen(tokenId.arr, tokenId.arr, tokenId.arr, 1)
    description <- validDescStringGen
    validCreate <- registerVSwapGen(master, validContract, data, description, fee, ts + 1)
    wrongOpcCreate <- registerVSwapGen(master, wrongOpcContract, data, description, fee + 10000000000L, ts+3)
    wrongDataCreate <- registerVSwapGen(master, wrongDataContract, data, description, fee + 10000000000L, ts+3)
  } yield (validCreate, wrongOpcCreate, wrongDataCreate)

  property("register contract transaction cannot pass due to wrong opcode"){
    forAll(preconditionsAndRegContractWrongFun) { case (validCreate, wrongOpcCreate, _) =>
      assertOpcFuncDifferEi(2, None, validCreate) { opcFunDiffEi =>
        opcFunDiffEi shouldBe an[Right[_, _]]
      }

      assertOpcFuncDifferEi(2, None, wrongOpcCreate) { opcFunDiffEi =>
        opcFunDiffEi shouldBe Left(ContractInvalidOPCData)
      }
    }
  }

  property("register contract transaction cannot pass due to wrong list of parameters"){
    forAll(preconditionsAndRegContractWrongFun) { case (_, _, wrongDataCreate) =>
      assertOpcFuncDifferEi(2, None, wrongDataCreate) { opcFunDiffEi =>
        opcFunDiffEi shouldBe Left(ContractDataTypeMismatch)
      }
    }
  }
}




