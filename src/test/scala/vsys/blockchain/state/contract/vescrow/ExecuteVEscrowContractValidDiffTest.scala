package vsys.blockchain.state.contract.vescrow

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.token.SystemContractGen
import vsys.blockchain.contract.vescrow.{VEscrowContractGen, VEscrowFunctionHelperGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.state._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import com.google.common.primitives.{Bytes, Ints, Longs}
import vsys.account.ContractAccount
import vsys.account.ContractAccount.tokenIdFromBytes

class ExecuteVEscrowContractValidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with SystemContractGen
  with VEscrowContractGen
  with VEscrowFunctionHelperGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndExecuteContractEscrow: Gen[(GenesisTransaction, GenesisTransaction, GenesisTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
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
    judgeDepositVSYSData = Seq(judge.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000L))
    recipientDepositVSYSData = Seq(recipient.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000L))
    payerDepositVSYSData = Seq(payer.toAddress.bytes.arr, regVEscrow.contractId.bytes.arr, Longs.toByteArray(1000L))
    depositVSYSDataType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    judgeDepositVSYS <- depositVSYSGen(judge, judgeDepositVSYSData, depositVSYSDataType, attach, fee, ts + 1)
    recipientDepositVSYS <- depositVSYSGen(recipient, recipientDepositVSYSData, depositVSYSDataType, attach, fee, ts + 2)
    payerDepositVSYS <- depositVSYSGen(payer, payerDepositVSYSData, depositVSYSDataType, attach, fee, ts + 3)
    withdrawVSYSData = Seq(regVEscrow.contractId.bytes.arr, judge.toAddress.bytes.arr, Longs.toByteArray(1000L))
    withdrawVSYSDataType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    judgeWithdrawVSYS <- withdrawVSYSGen(judge, withdrawVSYSData, withdrawVSYSDataType, attach, fee, ts + 4)
    createVEscrow <- createVEscrowGen(payer, regVEscrow.contractId, recipient.toAddress, 1000L, 1000L, 1000L, 10L, 10L, ts + 100L, attach, fee, ts + 5)
    recipientDepositToOrder <- recipientDepositVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 6)
    judgeDepositToOrder <- judgeDepositVEscrowGen(judge, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 7)
    payerCancelOrder <- payerCancelVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 8)
    recipientCancelOrder <- recipientCancelVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 10)
    judgeCancelOrder <- judgeCancelVEscrowGen(judge, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 11)
    submitWork <- submitWorkVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 12)
    approveWork <- approveWorkVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 13)
    applyToJudge <- applyToJudgeVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach,fee, ts + 14)
    judgeWork <- judgeVEscrowGen(judge, regVEscrow.contractId, createVEscrow.id.arr,0L, 1990L, attach, fee, ts + 15)
    submitPenalty <- submitPenaltyVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 16)
    payerRefund <- payerRefundVEscrowGen(payer, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 17)
    recipientRefund <- recipientRefundVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 18)
    recipientCollect <- collectVEscrowGen(recipient, regVEscrow.contractId, createVEscrow.id.arr, attach, fee, ts + 19)
  } yield (genesis, genesis2, genesis3, regVEscrow, judgeDepositVSYS, recipientDepositVSYS, payerDepositVSYS, judgeWithdrawVSYS,
    createVEscrow, recipientDepositToOrder, judgeDepositToOrder, payerCancelOrder, recipientCancelOrder, judgeCancelOrder, submitWork, approveWork,
    applyToJudge, judgeWork, submitPenalty, payerRefund, recipientRefund, recipientCollect)

  property("v-escrow able to deposit and withdraw VSYS") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, _, _, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, _, _, judgeWithdrawVSYS: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _,
    _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS))),
        TestBlock.createWithTxStatus(judgeWithdrawVSYS.timestamp, Seq(judgeWithdrawVSYS), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val tokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateVarKeys = getEscrowContractStateVarKeys(regVEscrow.contractId.bytes.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 0L

        newState.contractInfo(contractStateVarKeys.head) shouldEqual Some(DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get)
        newState.contractInfo(contractStateVarKeys(1)) shouldEqual Some(DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get)
        newState.contractInfo(contractStateVarKeys(2)) shouldEqual Some(DataEntry.create(tokenId.arr, DataType.TokenId).right.get)
        newState.contractInfo(contractStateVarKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000000000L), DataType.Timestamp).right.get)
        newState.contractInfo(contractStateVarKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000000000L), DataType.Timestamp).right.get)
      }
    }
  }

  property("v-escrow able to register and create") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, _, payerDepositVSYS: ExecuteContractFunctionTransaction, _, createVEscrow: ExecuteContractFunctionTransaction, _, _, _, _, _, _,
    _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS, payerDepositVSYS))),
        TestBlock.createWithTxStatus(createVEscrow.timestamp, Seq(createVEscrow), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 1000L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 0L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(9)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // orderExpirationTime
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(0L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(0L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow able to deposit to order") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction, _, _, _, _,
    _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow))),
        TestBlock.createWithTxStatus(recipientDepositToOrder.timestamp, Seq(recipientDepositToOrder, judgeDepositToOrder), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 0L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 0L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 0L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(9)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // orderExpirationTime
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow payer able to cancel order") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, judgeDepositToOrder: ExecuteContractFunctionTransaction, payerCancelOrder: ExecuteContractFunctionTransaction, _, _, _, _,
    _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(payerCancelOrder.timestamp, Seq(payerCancelOrder), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 1000L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 1000L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 1000L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(9)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // orderExpirationTime
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(0L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow recipient able to cancel order") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, _, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, recipientCancelOrder: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositVSYS.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(recipientCancelOrder.timestamp, Seq(recipientCancelOrder), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 1000L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 1000L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 1000L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(9)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // orderExpirationTime
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(0L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow judge able to cancel order") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, _,
    _, _, judgeCancelOrder: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientDepositToOrder.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder))),
        TestBlock.createWithTxStatus(judgeCancelOrder.timestamp, Seq(judgeCancelOrder), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 1000L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 1000L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 1000L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(9)) shouldEqual Some(DataEntry.create(Longs.toByteArray(genesis.timestamp + 100), DataType.Timestamp).right.get) // orderExpirationTime
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(0L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow recipient able to submit work") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, submitWork: ExecuteContractFunctionTransaction, _, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(judgeDepositToOrder.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(submitWork.timestamp, Seq(submitWork), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 0L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 0L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 0L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow payer able to approve work") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, submitWork: ExecuteContractFunctionTransaction, approveWork: ExecuteContractFunctionTransaction, _, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(submitWork.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, submitWork))),
        TestBlock.createWithTxStatus(approveWork.timestamp, Seq(approveWork), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 1010L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 0L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 1990L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow payer able to apply to judge") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, submitWork: ExecuteContractFunctionTransaction, _, applyToJudge: ExecuteContractFunctionTransaction, _, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(submitWork.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, submitWork))),
        TestBlock.createWithTxStatus(applyToJudge.timestamp, Seq(applyToJudge), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 0L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 0L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 0L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow judge able to judge work") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, submitWork: ExecuteContractFunctionTransaction, _, applyToJudge: ExecuteContractFunctionTransaction, judgeWork: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientDepositToOrder.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, submitWork, applyToJudge))),
        TestBlock.createWithTxStatus(judgeWork.timestamp, Seq(judgeWork), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 1010L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 0
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 1990L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow payer able to submit penalty") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, _, _, _, _, submitPenalty: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientDepositToOrder.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder))),
        TestBlock.createWithTxStatus(submitPenalty.timestamp + 1000000000L, Seq(submitPenalty), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 1010L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 1990L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 0L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow payer able to refund") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, submitWork: ExecuteContractFunctionTransaction, _, applyToJudge: ExecuteContractFunctionTransaction, _, _, payerRefund: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(applyToJudge.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, submitWork, applyToJudge))),
        TestBlock.createWithTxStatus(payerRefund.timestamp + 1000000000L, Seq(payerRefund), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 0L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 10L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 2990L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow recipient able to refund") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, submitWork: ExecuteContractFunctionTransaction, _, applyToJudge: ExecuteContractFunctionTransaction, _, _, _,
    recipientRefund: ExecuteContractFunctionTransaction, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(applyToJudge.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, submitWork, applyToJudge))),
        TestBlock.createWithTxStatus(recipientRefund.timestamp + 1000000000L, Seq(recipientRefund), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 0L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 10L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 2990L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }

  property("v-escrow recipient able to collect") {
    forAll(preconditionsAndExecuteContractEscrow) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, genesis3: GenesisTransaction, regVEscrow: RegisterContractTransaction,
    judgeDepositVSYS: ExecuteContractFunctionTransaction, recipientDepositVSYS: ExecuteContractFunctionTransaction, payerDepositVSYS: ExecuteContractFunctionTransaction,
    _, createVEscrow: ExecuteContractFunctionTransaction, recipientDepositToOrder: ExecuteContractFunctionTransaction, judgeDepositToOrder: ExecuteContractFunctionTransaction,
    _, _, _, submitWork: ExecuteContractFunctionTransaction, _, _, _, _, _, _, recipientCollect: ExecuteContractFunctionTransaction) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2, genesis3)), TestBlock.create(recipientDepositToOrder.timestamp, Seq(regVEscrow, judgeDepositVSYS,
        recipientDepositVSYS, payerDepositVSYS, createVEscrow, recipientDepositToOrder, judgeDepositToOrder, submitWork))),
        TestBlock.createWithTxStatus(recipientCollect.timestamp + 1000000000L, Seq(recipientCollect), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success

        val judge = regVEscrow.proofs.firstCurveProof.explicitGet().publicKey
        val payer = payerDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey
        val recipient = recipientDepositVSYS.proofs.firstCurveProof.explicitGet().publicKey

        val judgeBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(judge.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val payerBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(payer.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val recipientBalanceKey = ByteStr(Bytes.concat(regVEscrow.contractId.bytes.arr, Array(0.toByte), DataEntry.create(recipient.toAddress.bytes.arr, DataType.Address).right.get.bytes))
        val contractStateMapKeys = getEscrowContractStateMapKeys(regVEscrow.contractId.bytes.arr, createVEscrow.id.arr)

        newState.contractNumInfo(judgeBalanceKey) shouldEqual 1010L
        newState.contractNumInfo(payerBalanceKey) shouldEqual 0L
        newState.contractNumInfo(recipientBalanceKey) shouldEqual 1990L

        newState.contractInfo(contractStateMapKeys.head) shouldEqual Some(DataEntry.create(genesis3.recipient.bytes.arr, DataType.Address).right.get) // orderPayer
        newState.contractInfo(contractStateMapKeys(1)) shouldEqual Some(DataEntry.create(genesis2.recipient.bytes.arr, DataType.Address).right.get) // orderRecipient
        newState.contractInfo(contractStateMapKeys(2)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderAmount
        newState.contractInfo(contractStateMapKeys(3)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRecipientDeposit
        newState.contractInfo(contractStateMapKeys(4)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeDeposit
        newState.contractInfo(contractStateMapKeys(5)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderFee
        newState.contractInfo(contractStateMapKeys(6)) shouldEqual Some(DataEntry.create(Longs.toByteArray(990L), DataType.Amount).right.get) // orderRecipientAmount
        newState.contractInfo(contractStateMapKeys(7)) shouldEqual Some(DataEntry.create(Longs.toByteArray(10L), DataType.Amount).right.get) // orderRefund
        newState.contractInfo(contractStateMapKeys(8)) shouldEqual Some(DataEntry.create(Longs.toByteArray(2990L), DataType.Amount).right.get) // orderRecipientRefund
        newState.contractInfo(contractStateMapKeys(10)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderStatus
        newState.contractInfo(contractStateMapKeys(11)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderRepDepositStatus
        newState.contractInfo(contractStateMapKeys(12)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderJudgeDepositStatus
        newState.contractInfo(contractStateMapKeys(13)) shouldEqual Some(DataEntry.create(Array(1.toByte), DataType.Boolean).right.get) // orderSubmitStatus
        newState.contractInfo(contractStateMapKeys(14)) shouldEqual Some(DataEntry.create(Array(0.toByte), DataType.Boolean).right.get) // orderJudgeStatus
        newState.contractInfo(contractStateMapKeys(15)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderRepLockedAmount
        newState.contractInfo(contractStateMapKeys(16)) shouldEqual Some(DataEntry.create(Longs.toByteArray(1000L), DataType.Amount).right.get) // orderJudgeLockedAmount
      }
    }
  }
}