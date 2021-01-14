
package vsys.blockchain.state.contract.voption

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract.ContractGenHelper.basicContractTestGen
import vsys.blockchain.contract.{DataEntry, DataType}
import vsys.blockchain.contract.token.SystemContractGen
import vsys.blockchain.contract.voption.{VOptionContractGen, VOptionFunctionHelperGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract.{RegisterContractTransaction => RC, ExecuteContractFunctionTransaction => EC}
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}

class ExecuteVOptionValidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with VOptionContractGen
  with VOptionFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVOptionDepositBaseToken: Gen[(GenesisTransaction, RC,
    RC, RC, RC, RC, EC, EC, EC, EC)] = for {
    // Generates 4 register token contract transactions and a register contract tx for V Option
    // Also generates 4 deposit functions, proof and option token deposits the entire supply, deposit amount for base and target tokens can be selected
    (genesis, genesis2, master, user, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, issueTargetToken, issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken , depositProofToken, fee, ts, attach) <-
      createBaseTargetOptionProofTokenAndInitVOption(
        1000L, // baseTotalSupply
        1L, // baseUnity
        1000L, // baseIssueAmount
        1000L, // targetTotalSupply
        1L, // targetUnity
        1000L, // targetIssueAmount
        1000L, // optionTotalSupply
        1L, // optionUnity
        1000L, // proofTotalSupply
        1L, // proofUnity
        100L, // baseTokenDepositAmount
        500L, // targetTokenDepositAmount
        1000L, // optionTokenDepositAmount
        1000L) // targetTokenDepositAmount

  } yield (genesis, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken,
    issueTargetToken, depositBaseToken, depositTargetToken)

  property("vOption able to deposit") {
    forAll(preconditionsAndVOptionDepositBaseToken) { case (genesis: GenesisTransaction, registerBase: RC,
    registerTarget: RC, registerOption: RC, registerProof: RC,
    registerVOption: RC, issueBase: EC, issueTarget: EC,
    depositBase: EC, depositTarget: EC) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)),
        TestBlock.create(registerVOption.timestamp, Seq(registerBase, registerTarget, registerOption, registerProof, registerVOption, issueBase, issueTarget,
          depositBase))),
        TestBlock.createWithTxStatus(depositTarget.timestamp, Seq(depositTarget), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = registerVOption.proofs.firstCurveProof.explicitGet().publicKey

        val (contractBaseTokenBalanceKey, contractTargetTokenBalanceKey, _, _) = getOptionContractTokenBalanceKeys(registerBase.contractId.bytes.arr,
          registerTarget.contractId.bytes.arr, registerOption.contractId.bytes.arr,
          registerProof.contractId.bytes.arr, registerVOption.contractId.bytes.arr)

        val (masterBaseTokenBalanceKey, masterTargetTokenBalanceKey, _, _) = getOptionUserTokenBalanceKeys(registerBase.contractId.bytes.arr,
          registerTarget.contractId.bytes.arr, registerOption.contractId.bytes.arr,
          registerProof.contractId.bytes.arr, master)

        newState.tokenAccountBalance(masterBaseTokenBalanceKey) shouldBe 900L // issue 1000, deposit 100 into contract
        newState.tokenAccountBalance(contractBaseTokenBalanceKey) shouldBe 100L // deposit 100 into contract

        newState.tokenAccountBalance(masterTargetTokenBalanceKey) shouldBe 500L // issue 1000, deposit 500 into contract
        newState.tokenAccountBalance(contractTargetTokenBalanceKey) shouldBe 500L // deposit 500 into contract
      }
    }
  }

  // test if we can deposit and withdraw all tokens to and from a v option contract
  // all 4 tokens deposit and withdraw has been tested locally, the unit test will test for withdrawing base token only

  val preconditionsAndVoptionWithdrawBaseToken: Gen[(GenesisTransaction, RC,
    RC, RC, RC, RC, EC, EC, EC)] = for {
    (genesis, _, master, _, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract,
    issueBaseToken, _, _, _, depositBaseToken, _, _ , _, fee, ts, _) <-
      createBaseTargetOptionProofTokenAndInitVOption(
        1000L, // baseTotalSupply
        1L, // baseUnity
        1000L, // baseIssueAmount
        1000L, // targetTotalSupply
        1L, // targetUnity
        1000L, // targetIssueAmount
        1000L, // optionTotalSupply
        1L, // optionUnity
        1000L, // proofTotalSupply
        1L, // proofUnity
        1000L, // baseTokenDepositAmount
        1000L, // targetTokenDepositAmount
        1000L, // optionTokenDepositAmount
        1000L) // proofTokenDepositAmount

    withdrawBaseToken <- withdrawToken(master, regBaseTokenContract.contractId, regVOptionContract.contractId.bytes.arr, master.toAddress.bytes.arr, 100L, fee, ts + 13)

  } yield (genesis, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, depositBaseToken,
    withdrawBaseToken)

  property("vOption able to withdraw") {
    forAll(preconditionsAndVoptionWithdrawBaseToken) { case (genesis: GenesisTransaction, registerBase: RC,
    registerTarget: RC, registerOption: RC, registerProof: RC,
    registerVOption: RC, issueBase: EC, depositBase: EC,
    withdrawBase: EC) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)),
        TestBlock.create(registerVOption.timestamp, Seq(registerBase, registerTarget, registerOption, registerProof, registerVOption, issueBase, depositBase))),
        TestBlock.createWithTxStatus(withdrawBase.timestamp, Seq(withdrawBase), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val master = registerVOption.proofs.firstCurveProof.explicitGet().publicKey

        val (contractBaseTokenBalanceKey, _, _, _) = getOptionContractTokenBalanceKeys(registerBase.contractId.bytes.arr,
          registerTarget.contractId.bytes.arr, registerOption.contractId.bytes.arr,
          registerProof.contractId.bytes.arr, registerVOption.contractId.bytes.arr)

        val (masterBaseTokenBalanceKey, _, _, _) = getOptionUserTokenBalanceKeys(registerBase.contractId.bytes.arr,
          registerTarget.contractId.bytes.arr, registerOption.contractId.bytes.arr,
          registerProof.contractId.bytes.arr, master)

        newState.tokenAccountBalance(masterBaseTokenBalanceKey) shouldBe 100L // withdraw 100
        newState.tokenAccountBalance(contractBaseTokenBalanceKey) shouldBe 900L // deposit 1000, withdraw 100
      }
    }
  }

  // testing logic of supersede, address registering the v option contract and activating it should be different
  // making the user address register and issue tokens reduces need for sending over tokens before activating

  val preconditionsAndVOptionSupersedeActivate: Gen[(GenesisTransaction, GenesisTransaction, RC,
    RC, RC, RC, RC, EC, EC, EC, EC, EC, EC, EC, EC, EC, EC)] = for {

    (master, ts, fee) <- basicContractTestGen()

    genesis <- genesisVOptionGen(master, ts)
    user <- accountGen
    genesis2 <- genesisVOptionGen(user, ts)
    vOptionContract <- vOptionContractGen()

    // register base token
    regBaseTokenContract <- registerToken(user, 1000L, 1L, "init", fee + 10000000000L, ts)
    baseTokenContractId = regBaseTokenContract.contractId
    baseTokenId = tokenIdFromBytes(baseTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register target token
    regTargetTokenContract <- registerToken(user, 1000L, 1L, "init", fee + 10000000000L, ts + 1)
    targetTokenContractId = regTargetTokenContract.contractId
    targetTokenId = tokenIdFromBytes(targetTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register option token
    regOptionTokenContract <- registerToken(user, 1000L, 1L, "init", fee + 10000000000L, ts + 2)
    optionTokenContractId = regOptionTokenContract.contractId
    optionTokenId = tokenIdFromBytes(optionTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    // register proof token
    regProofTokenContract <- registerToken(user, 1000L, 1L, "init", fee + 10000000000L, ts + 3)
    proofTokenContractId = regProofTokenContract.contractId
    proofTokenId = tokenIdFromBytes(proofTokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

    // register VSwap contract
    description <- validDescStringGen
    initVOptionDataStack: Seq[DataEntry] <- initVOptionDataStackGen(baseTokenId.arr, targetTokenId.arr, optionTokenId.arr, proofTokenId.arr, ts + 100, ts + 200)
    regVOptionContract <- registerVOptionGen(master, vOptionContract, initVOptionDataStack, description, fee + 10000000000L, ts + 4)
    vOptionContractId = regVOptionContract.contractId

    // issue base token
    attach <- genBoundedString(2, EC.MaxDescriptionSize)
    issueBaseToken <- issueToken(user, baseTokenContractId, 1000L, fee, ts + 5)
    // issue target token
    issueTargetToken <- issueToken(user, targetTokenContractId, 1000L, fee, ts + 6)
    // issue option token, issue the entire supply of option tokens
    issueOptionToken <- issueToken(user, optionTokenContractId, 1000L, fee, ts + 7)
    // issue proof token, issue the entire supply of proof tokens
    issueProofToken <- issueToken(user, proofTokenContractId, 1000L, fee, ts + 8)

    depositBaseToken <- depositToken(user, baseTokenContractId, user.toAddress.bytes.arr, vOptionContractId.bytes.arr, 1000L, fee + 10000000000L, ts + 9)
    depositTargetToken <- depositToken(user, targetTokenContractId, user.toAddress.bytes.arr, vOptionContractId.bytes.arr, 1000L, fee + 10000000000L, ts + 10)
    depositOptionToken <- depositToken(user, optionTokenContractId, user.toAddress.bytes.arr, vOptionContractId.bytes.arr, 1000L, fee + 10000000000L, ts + 11)
    depositProofToken <- depositToken(user, proofTokenContractId, user.toAddress.bytes.arr, vOptionContractId.bytes.arr, 1000L, fee + 10000000000L, ts + 12)

    supersedeOption <- supersedeVOptionGen(master, regVOptionContract.contractId, user.toAddress, attach, fee, ts + 13)
    activateOption <- activateVOptionGen(user, regVOptionContract.contractId, 1000L, 10L, 10L, attach, fee, ts + 14)
  } yield (genesis, genesis2, regBaseTokenContract, regTargetTokenContract, regOptionTokenContract, regProofTokenContract, regVOptionContract, issueBaseToken, issueTargetToken,
    issueOptionToken, issueProofToken, depositBaseToken, depositTargetToken, depositOptionToken, depositProofToken, supersedeOption, activateOption)

  property("vOption able to supersede and activate") {
    forAll(preconditionsAndVOptionSupersedeActivate) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, registerBase: RC, registerTarget: RC,
    registerOption: RC, registerProof: RC, registerVOption: RC, issueBase: EC,
    issueTarget: EC, issueOption: EC, issueProof: EC, depositBase: EC,
    depositTarget: EC, depositOption: EC, depositProof: EC,
    supersede: EC, activate: EC) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)),
        TestBlock.create(registerVOption.timestamp, Seq(registerBase, registerTarget, registerOption, registerProof, registerVOption, issueBase, issueTarget,
          issueOption, issueProof, depositBase, depositTarget, depositOption, depositProof))),
        TestBlock.createWithTxStatus(activate.timestamp, Seq(supersede, activate), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val user = registerBase.proofs.firstCurveProof.explicitGet().publicKey
        val vOptionContractId = registerVOption.contractId.bytes.arr

        val (optionStatusKey, maxIssueNumKey, reservedOptionKey,
        reservedProofKey, priceKey, priceUnitKey, tokenLockedKey, tokenCollectedKey) = getOptionContractStateVarKeys(vOptionContractId)

        val (userStateMapBaseTokenBalanceKey, userStateMapTargetTokenBalanceKey,
        userStateMapOptionTokenBalanceKey, userStateMapProofTokenBalanceKey) = getOptionContractStateMapKeys(vOptionContractId, user)

        newState.contractInfo(optionStatusKey) shouldBe Some(DataEntry(Array(1.toByte), DataType.Boolean))
        newState.contractInfo(maxIssueNumKey) shouldBe Some(DataEntry(Longs.toByteArray(1000L), DataType.Amount))
        newState.contractNumInfo(reservedOptionKey) shouldBe 1000L
        newState.contractNumInfo(reservedProofKey) shouldBe 1000L
        newState.contractInfo(priceKey) shouldBe Some(DataEntry(Longs.toByteArray(10L), DataType.Amount))
        newState.contractInfo(priceUnitKey) shouldBe Some(DataEntry(Longs.toByteArray(10L), DataType.Amount))
        newState.contractNumInfo(tokenLockedKey) shouldBe 0L
      }
    }
  }
}
