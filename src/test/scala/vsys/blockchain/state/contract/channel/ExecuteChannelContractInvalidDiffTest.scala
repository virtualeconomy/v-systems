package vsys.blockchain.state.contract.channel

import com.google.common.primitives.{Ints, Longs, Shorts}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.{ContractAccount, PrivateKeyAccount}
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.block.TestBlock
import vsys.blockchain.contract._
import vsys.blockchain.contract.channel.PaymentChannelContractGen
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.state._
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
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisPaymentChannelGen(master, ts)
    user <- accountGen
    genesis2 <- genesisPaymentChannelGen(user, ts)
    contract <- channelContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    // Register a payment channel that supports VSYS
    regContract <- registerPaymentChannelGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(100L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(1000L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    invalidCreate <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
    //signature = channelIdLength + channelId + Long
    paymentSignatureBytes = EllipticCurveImpl.sign(master, Shorts.toByteArray(invalidCreate.id.arr.length.toShort) ++ invalidCreate.id.arr ++ Longs.toByteArray(50L))
    executePaymentData = Seq(invalidCreate.id.arr, Longs.toByteArray(50L), paymentSignatureBytes)
    executePaymentType = Seq(DataType.ShortBytes, DataType.Amount, DataType.ShortBytes)
    executePayment <- executePaymentChannelGen(user, contractId, executePaymentData, executePaymentType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, invalidCreate, executePayment, fee, ts)

  property("Create payment channel with insufficient amount") {
    forAll(preconditionsAndPaymentChannelWithAmountInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, invalidCreate: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.createWithTxStatus(invalidCreate.timestamp + 1, Seq(invalidCreate), TransactionStatus.ContractMapValueInsufficient)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.ContractMapValueInsufficient
      }
    }
  }

  val preconditionsAndPaymentChannelWithAddressInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisPaymentChannelGen(master, ts)
    user <- byteArrayGen(48).map(seed => PrivateKeyAccount(seed))
    genesis2 <- genesisPaymentChannelGen(user, ts)
    contract <- channelContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    // Register a payment channel that supports VSYS
    regContract <- registerPaymentChannelGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(1000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    invalidCreate <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
    //signature = channelIdLength + channelId + Long
    paymentSignatureBytes = EllipticCurveImpl.sign(master, Shorts.toByteArray(invalidCreate.id.arr.length.toShort) ++ invalidCreate.id.arr ++ Longs.toByteArray(50L))
    executePaymentData = Seq(invalidCreate.id.arr, Longs.toByteArray(50L), paymentSignatureBytes)
    executePaymentType = Seq(DataType.ShortBytes, DataType.Amount, DataType.ShortBytes)
    executePayment <- executePaymentChannelGen(user, contractId, executePaymentData, executePaymentType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, invalidCreate, executePayment, fee, ts)

  property("Create payment channel with invalid recipient address") {
    forAll(preconditionsAndPaymentChannelWithAddressInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, invalidCreate: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.createWithTxStatus(invalidCreate.timestamp + 1, Seq(invalidCreate), TransactionStatus.Success)) { (blockDiff, newState) =>

        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  val preconditionsAndPaymentChannelWithTimeOrCallerInvalidTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisPaymentChannelGen(master, ts)
    user <- accountGen
    genesis2 <- genesisPaymentChannelGen(user, ts)
    contract <- channelContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    // Register a payment channel that supports VSYS
    regContract <- registerPaymentChannelGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(1000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts - 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
    chargeData = Seq(create.id.arr, Longs.toByteArray(100L))
    chargeType = Seq(DataType.ShortBytes, DataType.Amount)
    charge <- chargeChannelGen(master, contractId, chargeData, chargeType, attach, fee, ts)
    //signature = channelIdLength + channelId + Long
    executeWithdraw <- executeWithdrawChannelGen(master, contractId, create.id.arr, attach, fee, ts)
    invalidTerminateChannel <- terminateChannelGen(user, contractId, create.id.arr, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, create, charge, executeWithdraw, invalidTerminateChannel, fee, ts)

//  property("Create payment channel with invalid expired time") {
//    forAll(preconditionsAndPaymentChannelWithTimeOrCallerInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
//    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction, _, _, _) =>
//      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
//        TestBlock.createWithTxStatus(create.timestamp, Seq(create), TransactionStatus.Failed)) { (blockDiff, newState) =>
//
//        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
//      }
//    }
//  }

  property("Terminate the channel without the right") {
    forAll(preconditionsAndPaymentChannelWithTimeOrCallerInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction,
    invalidTerminateChannel: ExecuteContractFunctionTransaction, _, _) =>
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
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisPaymentChannelGen(master, ts)
    user <- accountGen
    genesis2 <- genesisPaymentChannelGen(user, ts)
    contract <- channelContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    // Register a payment channel that supports VSYS
    regContract <- registerPaymentChannelGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(1000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts+2)
    chargeData = Seq(create.id.arr, Longs.toByteArray(100L))
    chargeType = Seq(DataType.ShortBytes, DataType.Amount)
    charge <- chargeChannelGen(master, contractId, chargeData, chargeType, attach, fee, ts+3)
    //signature = channelIdLength + channelId + Long
    executeWithdraw <- executeWithdrawChannelGen(master, contractId, create.id.arr, attach, fee, ts+4)
    paymentSignatureBytes = EllipticCurveImpl.sign(master, Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr ++ Longs.toByteArray(10000L))
    executePaymentData = Seq(create.id.arr, Longs.toByteArray(10000L), paymentSignatureBytes)
    executePaymentType = Seq(DataType.ShortBytes, DataType.Amount, DataType.ShortBytes)
    executePayment <- executePaymentChannelGen(user, contractId, executePaymentData, executePaymentType, attach, fee, ts)
    terminateChannel <- terminateChannelGen(master, contractId, create.id.arr, attach, fee, ts+5)
  } yield (genesis, genesis2, regContract, depositVSYS, create, charge, executeWithdraw, terminateChannel, executePayment, fee, ts)

  property("Withdraw the amount before terminating") {
    forAll(preconditionsAndPaymentChannelWithWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction,
    terminateChannel: ExecuteContractFunctionTransaction, _, _, _) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create))),

        TestBlock.createWithTxStatus(create.timestamp + 1L, Seq(executeWithdraw), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }

  property("Execute the insufficient payment amount from the channel") {
    forAll(preconditionsAndPaymentChannelWithWithdrawInvalidTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction, executeWithdraw: ExecuteContractFunctionTransaction,
    terminateChannel: ExecuteContractFunctionTransaction, executePayment: ExecuteContractFunctionTransaction, _, _) =>

      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit, create))),

        TestBlock.createWithTxStatus(executePayment.timestamp + 1, Seq(executePayment), TransactionStatus.Failed)) { (blockDiff, newState) =>
        blockDiff.txsDiff.contractDB.isEmpty shouldBe true
        blockDiff.txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiff.txsDiff.portfolios.isEmpty shouldBe false
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Failed
      }
    }
  }
}
