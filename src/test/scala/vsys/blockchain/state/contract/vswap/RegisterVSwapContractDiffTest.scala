package vsys.blockchain.state.contract.vswap

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.token.TokenContractGen
import vsys.blockchain.contract.vswap.VswapContractGen
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract.RegisterContractTransaction
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen}

class RegisterVSwapContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with VswapContractGen
  with TokenContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val vSwapContract: Gen[Contract] = vSwapContractGen()
  val tContract: Gen[Contract] = tokenContractGen(false)
  val totalSupplyA: Long = 10000
  val totalSupplyB: Long = 10000
  val liquidityTotalSupply: Long = 1000
  val unityA: Long = 1
  val unityB: Long = 1
  val liquidityUnity: Long = 1

  val preconditionsAndVSwapWithTokenContractTest: Gen[(GenesisTransaction, RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, RegisterContractTransaction, Long)] = for {
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

    contract <- vSwapContract
    description <- validDescStringGen

    dataStack <- initVSwapContractDataStackGen(tokenAId.arr,tokenBId.arr,liquidityTokenId.arr, 100)
    regContract <- registerVSwapGen(master, contract, dataStack, description, fee + 10000000000L, ts+3)

    genesis <- genesisVSwapGen(master, ts)
  } yield (genesis, regTokenAContract, regTokenBContract, regLiquidityTokenContract, regContract, fee + 10000000000L)


  property("register VSwap contract function transactions doesn't break invariant") {
    forAll(preconditionsAndVSwapWithTokenContractTest) { case (genesis, regA: RegisterContractTransaction, regB: RegisterContractTransaction, regLiquidity: RegisterContractTransaction, regVswap: RegisterContractTransaction, fee:Long) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis, regA, regB, regLiquidity))), TestBlock.create(Seq(regVswap))) { (blockDiff, newState) =>

        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee
        val master = regVswap.proofs.firstCurveProof.explicitGet().publicKey
        val contractId = regVswap.contractId.bytes
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))

        val (_, masterTxs) = newState.accountTransactionIds(master, 2, 0)
        masterTxs.size shouldBe 2 // genesis, reg
        newState.contractContent(contractId) shouldEqual Some((2, regVswap.id, ContractVSwap.contract))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
      }
    }
  }
}
