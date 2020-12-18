package vsys.blockchain.state.contract.vswap

import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract.{Contract, ContractGenHelper, DataEntry}
import vsys.blockchain.contract.ContractVSwap.{addLiquidityFunc, depositTrigger, descriptorTextual, initTrigger, removeLiquidityFunc, setSwapFunc, stateMapSeq, stateMapTextual, stateVarSeq, stateVarTextual, supersedeFunc, swapExactTokenForBaseTokenFunc, swapExactTokenForTargetTokenFunc, swapTokenForExactBaseTokenFunc, swapTokenForExactTargetTokenFunc, triggerTextual, withdrawTrigger}
import vsys.utils.serialization.Deser
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.contract.vswap.{VswapContractGen, VswapFunction}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.ValidationError.{ContractDataTypeMismatch, ContractInvalidOPCData}
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}

class VswapContractOpcDiffTest extends PropSpec
    with PropertyChecks
    with GeneratorDrivenPropertyChecks
    with Matchers
    with TransactionGen
    with VswapContractGen
    with VswapFunction
    with TokenContractGen {

    private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

    val tContract: Gen[Contract] = tokenContractGen(false)
    val totalSupplyA: Long = 10000
    val totalSupplyB: Long = 10000
    val liquidityTotalSupply: Long = 1000
    val unityA: Long = 1
    val unityB: Long = 1
    val liquidityUnity: Long = 1

    val regWrongOpcFunContract: Gen[Contract] =
      Gen.const(Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
        wrongOpcTrigger, // Triggers
      Seq(supersedeFunc, setSwapFunc, addLiquidityFunc, removeLiquidityFunc, swapTokenForExactBaseTokenFunc,
        swapExactTokenForBaseTokenFunc, swapTokenForExactTargetTokenFunc, swapExactTokenForTargetTokenFunc), // Functions
      stateVarSeq, // StateVars
      stateMapSeq, // StateMaps
      Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)  // Textual
    ).explicitGet())


    val regWrongDataContract: Gen[Contract] =
      Gen.const(Contract.buildContract(Deser.serilizeString("vdds"), Ints.toByteArray(2),
        wrongDataTrigger, // Triggers
        Seq(supersedeFunc, setSwapFunc, addLiquidityFunc, removeLiquidityFunc, swapTokenForExactBaseTokenFunc,
          swapExactTokenForBaseTokenFunc, swapTokenForExactTargetTokenFunc, swapExactTokenForTargetTokenFunc), // Functions
        stateVarSeq, // StateVars
        stateMapSeq, // StateMaps
        Seq(triggerTextual, descriptorTextual, stateVarTextual, stateMapTextual)  // Textual
      ).explicitGet())

    val preconditionsAndRegContractWrongFun: Gen[(RegisterContractTransaction, RegisterContractTransaction)] = for {
      (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
      tokenAContract <- tContract
      tokenBContract <- tContract
      tokenLiquidityContract <- tContract

      /*** register token A ***/
      initADataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupplyA, unityA, "init")
      description <- validDescStringGen
      regTokenAContract <- registerTokenGen(master, tokenAContract, initADataStack, description, fee + 10000000000L, ts)
      tokenAContractId = regTokenAContract.contractId
      tokenAId = tokenIdFromBytes(tokenAContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

      /*** register token B ***/
      initBDataStack: Seq[DataEntry] <- initTokenDataStackGen(totalSupplyB, unityB, "init")
      description <- validDescStringGen
      regTokenBContract <- registerTokenGen(master, tokenBContract, initBDataStack, description, fee + 10000000000L, ts+1)
      tokenBContractId = regTokenBContract.contractId
      tokenBId = tokenIdFromBytes(tokenBContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

      /*** register liquidity token ***/
      initLiquidityDataStack: Seq[DataEntry] <- initTokenDataStackGen(liquidityTotalSupply, liquidityUnity, "init")
      description <- validDescStringGen
      regLiquidityTokenContract <- registerTokenGen(master, tokenLiquidityContract, initLiquidityDataStack, description, fee + 10000000000L, ts+2)
      liquidityTokenContractId = regLiquidityTokenContract.contractId
      liquidityTokenId = tokenIdFromBytes(liquidityTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

      contract <- regWrongOpcFunContract
      contract1 <- regWrongDataContract

      description <- validDescStringGen

      dataStack <- initVSwapContractDataStackGen(tokenAId.arr,tokenBId.arr,liquidityTokenId.arr, 100)
      create <- registerVSwapGen(master, contract, dataStack, description, fee + 10000000000L, ts+3)
      create1 <- registerVSwapGen(master, contract1, dataStack, description, fee + 10000000000L, ts+3)
    } yield (create, create1)

    property("register contract transaction cannot pass due to wrong opcode"){
      forAll(preconditionsAndRegContractWrongFun) { case (create, create1) =>
        assertOpcFuncDifferEi(2, None, create) { opcFunDiffEi =>
          opcFunDiffEi shouldBe Left(ContractInvalidOPCData)
        }
      }
    }

    property("register contract transaction cannot pass due to wrong list of parameters"){
      forAll(preconditionsAndRegContractWrongFun) { case (create, create1) =>
        assertOpcFuncDifferEi(2, None, create1) { opcFunDiffEi =>
          opcFunDiffEi shouldBe Left(ContractDataTypeMismatch)
        }
      }
    }
}




