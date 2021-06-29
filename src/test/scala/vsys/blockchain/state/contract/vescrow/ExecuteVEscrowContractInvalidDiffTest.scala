package vsys.blockchain.state.contract.vescrow

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.token.SystemContractGen
import vsys.blockchain.contract.vescrow.VEscrowContractGen
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import com.google.common.primitives.{Ints, Longs}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes

class ExecuteVEscrowContractInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with VEscrowContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndVEscrowInvalidDeposits: Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (judge, ts, fee) <- ContractGenHelper.basicContractTestGen()
    recipient <- accountGen
    payer <- accountGen
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis <- genesisVEscrowGen(judge, ts)
    genesis2 <- genesisVEscrowGen(recipient, ts)
    genesis3 <- genesisVEscrowGen(payer, ts)
    vEscrow <- vEscrowContractGen()
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    initVEscrowDataStack <- initVEscrowDataStackGen(tokenId.arr, 1000000000L, 1000000000L)
    regVEscrow <- registerVEscrowGen(judge, vEscrow, initVEscrowDataStack, "test", fee, ts)

    judgeDepositVSYSData = Seq(judge.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    recipientDepositVSYSData = Seq(recipient.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    payerDepositVSYSData = Seq(payer.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    depositVSYSDataType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    judgeDepositVSYS <- depositVSYSGen(judge, judgeDepositVSYSData, depositVSYSDataType, attach, fee, ts + 1)
    recipientDepositVSYS <- depositVSYSGen(recipient, recipientDepositVSYSData, depositVSYSDataType, attach, fee, ts + 2)
    payerDepositVSYS <- depositVSYSGen(payer, payerDepositVSYSData, depositVSYSDataType, attach, fee, ts + 3)

    withdrawVSYSData = Seq(regVEscrow.contractId.bytes.arr, judge.toAddress.bytes.arr, Longs.toByteArray(1000000000001L))
    withdrawVSYSDataType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    judgeWithdrawVSYS <- withdrawVSYSGen(judge, withdrawVSYSData, withdrawVSYSDataType, attach, fee, ts + 4)

    createVEscrow <- createVEscrowGen(payer, regVEscrow.contractId, recipient.toAddress, 1000L, 1000L, 1000L, 10L, 10L, ts + 100L, attach, fee, ts + 5)
    createVEscrowLargeDepositAmounts <- createVEscrowGen(payer, regVEscrow.contractId, recipient.toAddress, 1000L, 1000000000001L, 1000000000001L, 10L, 10L, ts + 100L, attach, fee, ts + 5)
    invalidCreateVEscrowLargeCreateAmount <- createVEscrowGen(payer, regVEscrow.contractId, recipient.toAddress, 1000000000001L, 1000L, 1000L, 10L, 10L, ts + 100L, attach, fee, ts + 5)

    recipientDepositToOrder <- recipientDepositVEscrowGen(recipient, regVEscrow.contractId, createVEscrowLargeDepositAmounts.id.arr, attach, fee, ts + 6)
    judgeDepositToOrder <- judgeDepositVEscrowGen(judge, regVEscrow.contractId, createVEscrowLargeDepositAmounts.id.arr, attach, fee, ts + 7)

    recipientSubmitWork <- submitWorkVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 8)

    payerSubmitPenalty <- submitPenaltyVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 9)

    approveWork <- approveWorkVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 10)

    applyToJudge <- applyToJudgeVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 11)

    recipientCollect <- collectVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 12)

    judgeOrder <- judgeVEscrowGen(judge, regVEscrow.contractId, createVEscrow.id.arr, 100, 100, attach, fee, ts + 13)

    payerRefundOrder <- payerRefundVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 14)

    recipientRefundOrder <- recipientRefundVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 15)

  } yield (genesis, genesis2, genesis3, regVEscrow, judgeDepositVSYS, recipientDepositVSYS, payerDepositVSYS, judgeWithdrawVSYS, createVEscrow, createVEscrowLargeDepositAmounts, invalidCreateVEscrowLargeCreateAmount,
    recipientDepositToOrder, judgeDepositToOrder, recipientSubmitWork, payerSubmitPenalty, approveWork, applyToJudge, recipientCollect, judgeOrder, payerRefundOrder, recipientRefundOrder)

  property("v-escrow unable to withdraw more than deposited") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, _, _, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, _, _, judgeWithdrawVSYS: ExecuteContractFunctionTransaction, _, _, _,
    _, _, _, _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS))),
        TestBlock.createWithTxStatus(judgeWithdrawVSYS.timestamp, Seq(judgeWithdrawVSYS), TransactionStatus.ContractTokenBalanceInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractTokenBalanceInsufficient
      }
    }
  }

  property("v-escrow unable to create without sufficient amount") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, _, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction, _, _, _, invalidCreateVEscrow: ExecuteContractFunctionTransaction,
    _, _, _, _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS))),
        TestBlock.createWithTxStatus(invalidCreateVEscrow.timestamp, Seq(invalidCreateVEscrow), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  property("v-escrow recipient unable to deposit without sufficient amount") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, _, createVEscrowLargeDepositAmounts: ExecuteContractFunctionTransaction, _, recipientDepositToOrder: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrowLargeDepositAmounts))),
        TestBlock.createWithTxStatus(recipientDepositToOrder.timestamp, Seq(recipientDepositToOrder), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  property("v-escrow judge unable to deposit without sufficient amount") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, _, createVEscrowLargeDepositAmounts: ExecuteContractFunctionTransaction, _, _, judgeDepositToOrder: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrowLargeDepositAmounts))),
        TestBlock.createWithTxStatus(judgeDepositToOrder.timestamp, Seq(judgeDepositToOrder), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  property("v-escrow recipient unable to submit before judge deposits") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, recipientSubmitWork: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(recipientSubmitWork.timestamp, Seq(recipientSubmitWork), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer unable to submit penalty before order deposits") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, _, payerSubmitPenalty: ExecuteContractFunctionTransaction, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(payerSubmitPenalty.timestamp, Seq(payerSubmitPenalty), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer unable to approve before order deposits") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, _, _, approveWork: ExecuteContractFunctionTransaction, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(approveWork.timestamp, Seq(approveWork), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer unable to apply to judge before order deposits") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, applyToJudge: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(applyToJudge.timestamp, Seq(applyToJudge), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow recipient cannot collect before order deposits") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, _, recipientCollect: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(recipientCollect.timestamp, Seq(recipientCollect), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow cannnot judge before order deposits") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, _, _, judgeOrder: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(judgeOrder.timestamp, Seq(judgeOrder), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer cannot refund before order deposits") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, _, _, _, payerRefundOrder: ExecuteContractFunctionTransaction, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(payerRefundOrder.timestamp, Seq(payerRefundOrder), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow recipient cannot refund before order deposits") {
    forAll(preconditionsAndVEscrowInvalidDeposits) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, _, _, _, _, recipientRefundOrder: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(recipientRefundOrder.timestamp, Seq(recipientRefundOrder), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVEscrowInvalidSubmit: Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction)] = for {
    (judge, ts, fee) <- ContractGenHelper.basicContractTestGen()
    recipient <- accountGen
    payer <- accountGen
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis <- genesisVEscrowGen(judge, ts)
    genesis2 <- genesisVEscrowGen(recipient, ts)
    genesis3 <- genesisVEscrowGen(payer, ts)
    vEscrow <- vEscrowContractGen()
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    initVEscrowDataStack <- initVEscrowDataStackGen(tokenId.arr, 1000000000L, 1000000000L)
    regVEscrow <- registerVEscrowGen(judge, vEscrow, initVEscrowDataStack, "test", fee, ts)

    judgeDepositVSYSData = Seq(judge.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    recipientDepositVSYSData = Seq(recipient.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    payerDepositVSYSData = Seq(payer.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    depositVSYSDataType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    judgeDepositVSYS <- depositVSYSGen(judge, judgeDepositVSYSData, depositVSYSDataType, attach, fee, ts + 1)
    recipientDepositVSYS <- depositVSYSGen(recipient, recipientDepositVSYSData, depositVSYSDataType, attach, fee, ts + 2)
    payerDepositVSYS <- depositVSYSGen(payer, payerDepositVSYSData, depositVSYSDataType, attach, fee, ts + 3)

    createVEscrow <- createVEscrowGen(payer, regVEscrow.contractId, recipient.toAddress, 1000L, 1000L, 1000L, 10L, 10L, ts + 100L, attach, fee, ts + 4)

    recipientDepositToOrder <- recipientDepositVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 5)
    judgeDepositToOrder <- judgeDepositVEscrowGen(judge, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 6)

    approveWork <- approveWorkVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 7)

    applyToJudge <- applyToJudgeVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 8)

    recipientCollect <- collectVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 9)

    judgeOrder <- judgeVEscrowGen(judge, regVEscrow.contractId, createVEscrow.id.arr, 100, 100, attach, fee, ts + 10)

    payerRefundOrder <- payerRefundVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 11)

    recipientRefundOrder <- recipientRefundVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 12)

  } yield (genesis, genesis2, genesis3, regVEscrow, judgeDepositVSYS, recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder,
    approveWork, applyToJudge, recipientCollect, judgeOrder, payerRefundOrder, recipientRefundOrder)

  property("v-escrow judge cannot approve work before recipient submits work") {
    forAll(preconditionsAndVEscrowInvalidSubmit) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    approveWork: ExecuteContractFunctionTransaction, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, recipientDepositVSYS, createVEscrow,
        recipientDepositToOrder, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(approveWork.timestamp, Seq(approveWork), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer cannot apply to judge before recipient submits work") {
    forAll(preconditionsAndVEscrowInvalidSubmit) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, applyToJudge: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, recipientDepositVSYS, createVEscrow,
        recipientDepositToOrder, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(applyToJudge.timestamp, Seq(applyToJudge), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow recipient cannot collect before recipient submits work") {
    forAll(preconditionsAndVEscrowInvalidSubmit) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, recipientCollect: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, recipientDepositVSYS, createVEscrow,
        recipientDepositToOrder, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(recipientCollect.timestamp, Seq(recipientCollect), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow judge cannot judge order before recipient submits work") {
    forAll(preconditionsAndVEscrowInvalidSubmit) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, judgeOrder: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, recipientDepositVSYS, createVEscrow,
        recipientDepositToOrder, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(judgeOrder.timestamp, Seq(judgeOrder), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer cannot refund order before recipient submits work") {
    forAll(preconditionsAndVEscrowInvalidSubmit) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, _, payerRefundOrder: ExecuteContractFunctionTransaction, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, recipientDepositVSYS, createVEscrow,
        recipientDepositToOrder, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(payerRefundOrder.timestamp, Seq(payerRefundOrder), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow recipient cannot refund order before recipient submits work") {
    forAll(preconditionsAndVEscrowInvalidSubmit) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, _, _, recipientRefundOrder: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS, recipientDepositVSYS, createVEscrow,
        recipientDepositToOrder, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(recipientRefundOrder.timestamp, Seq(recipientRefundOrder), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  val preconditionsAndVEscrowInvalidTimestamps: Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction)] = for {
    (judge, ts, fee) <- ContractGenHelper.basicContractTestGen()
    recipient <- accountGen
    payer <- accountGen
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    genesis <- genesisVEscrowGen(judge, ts)
    genesis2 <- genesisVEscrowGen(recipient, ts)
    genesis3 <- genesisVEscrowGen(payer, ts)
    vEscrow <- vEscrowContractGen()
    tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    initVEscrowDataStack <- initVEscrowDataStackGen(tokenId.arr, 1000000000L, 1000000000L)
    regVEscrow <- registerVEscrowGen(judge, vEscrow, initVEscrowDataStack, "test", fee, ts)

    judgeDepositVSYSData = Seq(judge.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    recipientDepositVSYSData = Seq(recipient.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    payerDepositVSYSData = Seq(payer.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000000000000L))
    depositVSYSDataType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    judgeDepositVSYS <- depositVSYSGen(judge, judgeDepositVSYSData, depositVSYSDataType, attach, fee, ts + 1)
    recipientDepositVSYS <- depositVSYSGen(recipient, recipientDepositVSYSData, depositVSYSDataType, attach, fee, ts + 2)
    payerDepositVSYS <- depositVSYSGen(payer, payerDepositVSYSData, depositVSYSDataType, attach, fee, ts + 3)

    createVEscrow <- createVEscrowGen(payer, regVEscrow.contractId, recipient.toAddress, 1000L, 1000L, 1000L, 10L, 10L, ts + 100L, attach, fee, ts + 5)

    recipientDepositToOrder <- recipientDepositVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 6)
    judgeDepositToOrder <- judgeDepositVEscrowGen(judge, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 7)

    recipientSubmitWork <- submitWorkVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 8)

    payerSubmitPenalty <- submitPenaltyVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 9)

    approveWork <- approveWorkVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 10)

    applyToJudge <- applyToJudgeVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 11)

    recipientCollect <- collectVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 12)

    judgeOrder <- judgeVEscrowGen(judge, regVEscrow.contractId, createVEscrow.id.arr, 100, 100, attach, fee, ts + 13)

    payerRefundOrder <- payerRefundVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 14)

    recipientRefundOrder <- recipientRefundVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 15)

  } yield (genesis, genesis2, genesis3, regVEscrow, judgeDepositVSYS, recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder,
    judgeDepositToOrder, recipientSubmitWork, payerSubmitPenalty, approveWork, applyToJudge, recipientCollect, judgeOrder, payerRefundOrder, recipientRefundOrder)

  property("v-escrow payer cannot submit penalty before the order expires") {
    forAll(preconditionsAndVEscrowInvalidTimestamps) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction,
    payerDepositVSYS: ExecuteContractFunctionTransaction, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction,
    judgeDepositToOrder: ExecuteContractFunctionTransaction, recipientSubmitWork: ExecuteContractFunctionTransaction, payerSubmitPenalty: ExecuteContractFunctionTransaction,
    _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        payerDepositVSYS, recipientDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, recipientSubmitWork))),
        TestBlock.createWithTxStatus(payerSubmitPenalty.timestamp, Seq(payerSubmitPenalty), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer cannot approve the order after it expires") {
    forAll(preconditionsAndVEscrowInvalidTimestamps) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction,
    payerDepositVSYS: ExecuteContractFunctionTransaction, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction,
    judgeDepositToOrder: ExecuteContractFunctionTransaction, recipientSubmitWork: ExecuteContractFunctionTransaction, _,
    approveWork: ExecuteContractFunctionTransaction, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientSubmitWork.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        payerDepositVSYS, recipientDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, recipientSubmitWork))),
        TestBlock.createWithTxStatus(approveWork.timestamp + 1000000000L, Seq(approveWork), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer cannot apply to judge once order expires") {
    forAll(preconditionsAndVEscrowInvalidTimestamps) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction,
    payerDepositVSYS: ExecuteContractFunctionTransaction, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction,
    judgeDepositToOrder: ExecuteContractFunctionTransaction, recipientSubmitWork: ExecuteContractFunctionTransaction, _,
    _, applyToJudge: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientSubmitWork.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        payerDepositVSYS, recipientDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, recipientSubmitWork))),
        TestBlock.createWithTxStatus(applyToJudge.timestamp + 1000000000L, Seq(applyToJudge), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow recipient cannot collect before order expires") {
    forAll(preconditionsAndVEscrowInvalidTimestamps) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction,
    payerDepositVSYS: ExecuteContractFunctionTransaction, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction,
    judgeDepositToOrder: ExecuteContractFunctionTransaction, recipientSubmitWork: ExecuteContractFunctionTransaction, _,
    _, _, recipientCollect: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientSubmitWork.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        payerDepositVSYS, recipientDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, recipientSubmitWork))),
        TestBlock.createWithTxStatus(recipientCollect.timestamp, Seq(recipientCollect), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow judge cannot judge order after it expires") {
    forAll(preconditionsAndVEscrowInvalidTimestamps) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction,
    payerDepositVSYS: ExecuteContractFunctionTransaction, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction,
    judgeDepositToOrder: ExecuteContractFunctionTransaction, recipientSubmitWork: ExecuteContractFunctionTransaction, _,
    _, _, _, judgeOrder: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientSubmitWork.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        payerDepositVSYS, recipientDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, recipientSubmitWork))),
        TestBlock.createWithTxStatus(judgeOrder.timestamp, Seq(judgeOrder), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow payer cannot refund order before it expires") {
    forAll(preconditionsAndVEscrowInvalidTimestamps) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction,
    payerDepositVSYS: ExecuteContractFunctionTransaction, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction,
    judgeDepositToOrder: ExecuteContractFunctionTransaction, recipientSubmitWork: ExecuteContractFunctionTransaction, _,
    _, _, _, _, payerRefundOrder: ExecuteContractFunctionTransaction, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientSubmitWork.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        payerDepositVSYS, recipientDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, recipientSubmitWork))),
        TestBlock.createWithTxStatus(payerRefundOrder.timestamp, Seq(payerRefundOrder), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("v-escrow recipient cannot refund order before it expires") {
    forAll(preconditionsAndVEscrowInvalidTimestamps) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction,
    regVEscrow: RegisterContractTransaction, judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction,
    payerDepositVSYS: ExecuteContractFunctionTransaction, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction,
    judgeDepositToOrder: ExecuteContractFunctionTransaction, recipientSubmitWork: ExecuteContractFunctionTransaction, _,
    _, _, _, _, _, recipientRefundOrder: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientSubmitWork.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        payerDepositVSYS, recipientDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, recipientSubmitWork))),
        TestBlock.createWithTxStatus(recipientRefundOrder.timestamp, Seq(recipientRefundOrder), TransactionStatus.Failed)) { (blockDiff, _) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}