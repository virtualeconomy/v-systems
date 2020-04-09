package vsys.blockchain.state.contract.channel

import com.google.common.primitives.{Longs, Shorts}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.channel.PaymentChannelContractGen
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.contract._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.utils.crypto.EllipticCurveImpl

class ExecuteChannelContractInvalidDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with PaymentChannelContractGen {

  val preconditionsAndPaymentChannelWithAmountInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, create, ts, fee, description, attach) <- createAndDepositVSYSPaymentChannelGen(10L, 10L, 1000000000000L)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(1000L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    invalidCreate <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, create, invalidCreate, fee, ts)

  // invalid
  property("Create payment channel with insufficient amount") {
    forAll(preconditionsAndPaymentChannelWithAmountInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, invalidCreate: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.createWithTxStatus(invalidCreate.timestamp + 1, Seq(invalidCreate), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  // valid
  property("Create payment channel with sufficient amount") {
    forAll(preconditionsAndPaymentChannelWithAmountInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, invalidCreate: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.createWithTxStatus(create.timestamp + 1, Seq(create), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndPaymentChannelWithAddressInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, create, ts, fee, description, attach) <- createAndDepositVSYSPaymentChannelGen(1000L, 100L, 1000000000000L)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    createData = Seq(ContractAccount.systemContractId.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.ContractAccount, DataType.Amount, DataType.Timestamp)
    invalidCreate <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, create, invalidCreate, fee, ts)

  // valid
  property("Create payment channel with valid recipient address") {
    forAll(preconditionsAndPaymentChannelWithAddressInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, invalidCreate: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.createWithTxStatus(create.timestamp + 1, Seq(create), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  // invalid
  property("Create payment channel with invalid recipient address") {
    forAll(preconditionsAndPaymentChannelWithAddressInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, invalidCreate: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.createWithTxStatus(invalidCreate.timestamp + 1, Seq(invalidCreate), TransactionStatus.ContractDataTypeMismatch)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractDataTypeMismatch
      }
    }
  }

  val preconditionsAndPaymentChannelWithTimeOrChargeOrCallerInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, create, ts, fee, description, attach) <- createAndDepositVSYSPaymentChannelGen(1000L, 100L, 1000000000001L)
    contractId = regContract.contractId

    chargeData = Seq(create.id.arr, Longs.toByteArray(100L))
    chargeType = Seq(DataType.ShortBytes, DataType.Amount)
    charge <- chargeChannelGen(master, contractId, chargeData, chargeType, attach, fee, ts+3)

    chargeData = Seq(create.id.arr, Longs.toByteArray(10000L))
    invalidCharge <- chargeChannelGen(master, contractId, chargeData, chargeType, attach, fee, ts+3)

    updateTimeData = Seq(create.id.arr, Longs.toByteArray(ts + 1000000000002L))
    updateTimeType = Seq(DataType.ShortBytes, DataType.Timestamp)
    updateTime <- updateExpiredTimeChannelGen(master, contractId, updateTimeData, updateTimeType, attach, fee, ts + 3)

    updateTimeData = Seq(create.id.arr, Longs.toByteArray(ts + 1000000000000L))
    invalidUpdateTime <- updateExpiredTimeChannelGen(master, contractId, updateTimeData, updateTimeType, attach, fee, ts + 3)

    terminateChannel <- terminateChannelGen(master, contractId, create.id.arr, attach, fee, ts + 4)
    invalidTerminateChannel <- terminateChannelGen(user, contractId, create.id.arr, attach, fee, ts + 4)

  } yield (genesis, genesis2, regContract, depositVSYS, create, charge, invalidCharge, updateTime, invalidUpdateTime, terminateChannel, invalidTerminateChannel, fee, ts)

  // valid
  property("Execute update expired time in token payment channel later than current one") {
    forAll(preconditionsAndPaymentChannelWithTimeOrChargeOrCallerInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, _, updateTime: ExecuteContractFunctionTransaction, invalidUpdateTime: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create))),
        TestBlock.createWithTxStatus(updateTime.timestamp, Seq(updateTime), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  // invalid
  property("Execute update expired time in token payment channel earlier than current one") {
    forAll(preconditionsAndPaymentChannelWithTimeOrChargeOrCallerInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, _, updateTime: ExecuteContractFunctionTransaction, invalidUpdateTime: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create))),
        TestBlock.createWithTxStatus(invalidUpdateTime.timestamp, Seq(invalidUpdateTime), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // valid
  property("Charge amount from token payment channel") {
    forAll(preconditionsAndPaymentChannelWithTimeOrChargeOrCallerInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, _, _, invalidUpdateTime: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create))),
        TestBlock.createWithTxStatus(charge.timestamp, Seq(charge), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  // invalid
  property("Charge too much amount from token payment channel") {
    forAll(preconditionsAndPaymentChannelWithTimeOrChargeOrCallerInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, _, invalidCharge: ExecuteContractFunctionTransaction, _, invalidUpdateTime: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create))),
        TestBlock.createWithTxStatus(invalidCharge.timestamp, Seq(invalidCharge), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  // valid
  property("Terminate the channel with the right") {
    forAll(preconditionsAndPaymentChannelWithTimeOrChargeOrCallerInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, _, _, invalidUpdateTime: ExecuteContractFunctionTransaction,
    terminateChannel: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create))),
        TestBlock.createWithTxStatus(terminateChannel.timestamp + 1, Seq(terminateChannel), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  // invalid
  property("Terminate the channel without the right") {
    forAll(preconditionsAndPaymentChannelWithTimeOrChargeOrCallerInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, _, _, invalidUpdateTime: ExecuteContractFunctionTransaction,
    _, invalidTerminateChannel: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create))),
        TestBlock.createWithTxStatus(invalidTerminateChannel.timestamp + 1, Seq(invalidTerminateChannel), TransactionStatus.ContractInvalidCaller)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidCaller
      }
    }
  }

  val preconditionsAndPaymentChannelWithWithdrawInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (genesis, genesis2, master, user, regContract, depositVSYS, create, ts, fee, description, attach) <- createAndDepositVSYSPaymentChannelGen(1000L, 100L, 1000000000000L)
    contractId = regContract.contractId
    chargeData = Seq(create.id.arr, Longs.toByteArray(100L))
    chargeType = Seq(DataType.ShortBytes, DataType.Amount)
    charge <- chargeChannelGen(master, contractId, chargeData, chargeType, attach, fee, ts+3)
    executeWithdraw <- executeWithdrawChannelGen(master, contractId, create.id.arr, attach, fee, ts+5)

    paymentSignatureBytes1 = EllipticCurveImpl.sign(master, Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr ++ Longs.toByteArray(10L))
    paymentSignatureBytes2 = EllipticCurveImpl.sign(master, Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr ++ Longs.toByteArray(10000L))

    executePaymentData = Seq(create.id.arr, Longs.toByteArray(10L), paymentSignatureBytes1)
    executePaymentType = Seq(DataType.ShortBytes, DataType.Amount, DataType.ShortBytes)
    executePayment <- executePaymentChannelGen(user, contractId, executePaymentData, executePaymentType, attach, fee, ts)

    executePaymentData = Seq(create.id.arr, Longs.toByteArray(10000L), paymentSignatureBytes2)
    invalidExecutePayment <- executePaymentChannelGen(user, contractId, executePaymentData, executePaymentType, attach, fee, ts)

    executePaymentData = Seq(create.id.arr, Longs.toByteArray(10L), paymentSignatureBytes2)
    invalidSignatureExecutePayment <- executePaymentChannelGen(user, contractId, executePaymentData, executePaymentType, attach, fee, ts)

    terminateChannel <- terminateChannelGen(master, contractId, create.id.arr, attach, fee, ts + 4)
  } yield (genesis, genesis2, regContract, depositVSYS, create, charge, executeWithdraw, executePayment, invalidExecutePayment, invalidSignatureExecutePayment, terminateChannel, fee, ts)

  // valid
  property("Withdraw the amount after terminating") {
    forAll(preconditionsAndPaymentChannelWithWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction, _, _, _, terminateChannel: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create, charge, terminateChannel))),
        TestBlock.createWithTxStatus(terminateChannel.timestamp + 1000000000001L, Seq(executeWithdraw), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  // invalid
  property("Withdraw the amount before terminating") {
    forAll(preconditionsAndPaymentChannelWithWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction, _, _, _, terminateChannel: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create, charge))),
        TestBlock.createWithTxStatus(create.timestamp + 1L, Seq(executeWithdraw), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // invalid
  property("Withdraw the amount immediately after terminating") {
    forAll(preconditionsAndPaymentChannelWithWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction, _, _, _, terminateChannel: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create, charge, terminateChannel))),
        TestBlock.createWithTxStatus(terminateChannel.timestamp + 1L, Seq(executeWithdraw), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // valid
  property("Collect the payment amount from the channel") {
    forAll(preconditionsAndPaymentChannelWithWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction,
    executePayment: ExecuteContractFunctionTransaction, invalidExecutePayment: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create, charge))),
        TestBlock.createWithTxStatus(executePayment.timestamp + 1, Seq(executePayment), TransactionStatus.Success)) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  // invalid
  property("Collect the insufficient payment amount from the channel") {
    forAll(preconditionsAndPaymentChannelWithWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction,
    executePayment: ExecuteContractFunctionTransaction, invalidExecutePayment: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create, charge))),
        TestBlock.createWithTxStatus(invalidExecutePayment.timestamp + 1, Seq(invalidExecutePayment), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  // invalid
  property("Collect the payment amount from the channel with wrong signature") {
    forAll(preconditionsAndPaymentChannelWithWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction,
    executePayment: ExecuteContractFunctionTransaction, invalidExecutePayment: ExecuteContractFunctionTransaction, invalidSignatureExecutePayment: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create, charge))),
        TestBlock.createWithTxStatus(invalidSignatureExecutePayment.timestamp + 1, Seq(invalidSignatureExecutePayment), TransactionStatus.ContractInvalidSignature)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractInvalidSignature
      }
    }
  }
}