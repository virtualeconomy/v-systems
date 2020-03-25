package vsys.blockchain.state.contract.channel

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import vsys.account.ContractAccount
import vsys.blockchain.block.TestBlock
import vsys.account.ContractAccount.tokenIdFromBytes
import vsys.blockchain.contract._
import vsys.blockchain.contract.channel.PaymentChannelContractGen
import vsys.blockchain.contract.token.{SystemContractGen, TokenContractGen}
import vsys.blockchain.state._
import vsys.blockchain.state.diffs._
import vsys.blockchain.transaction.{GenesisTransaction, TransactionGen, TransactionStatus}
import vsys.blockchain.transaction.contract._
import vsys.utils.crypto.EllipticCurveImpl

class PaymentChannelContractDiffTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with TokenContractGen
  with SystemContractGen
  with PaymentChannelContractGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val channelContract: Gen[Contract] = paymentChannelContractGen()
  val tokenContract: Gen[Contract] = tokenContractGen(false)

  val preconditionsAndCreatePaymentChannelContractTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
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
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
    chargeData = Seq(create.id.arr, Longs.toByteArray(100L))
    chargeType = Seq(DataType.ShortBytes, DataType.Amount)
    charge <- chargeChannelGen(master, contractId, chargeData, chargeType, attach, fee, ts)
    //signature = channelIdLength + channelId + Long
    paymentSignatureBytes = EllipticCurveImpl.sign(master, Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr ++ Longs.toByteArray(50L))
    executePaymentData = Seq(create.id.arr, Longs.toByteArray(50L), paymentSignatureBytes)
    executePaymentType = Seq(DataType.ShortBytes, DataType.Amount, DataType.ShortBytes)
    executePayment <- executePaymentChannelGen(user, contractId, executePaymentData, executePaymentType, attach, fee, ts)
  } yield (genesis, genesis2, regContract, depositVSYS, create, charge, executePayment, fee, ts)

  property("Execute payment channel doesn't break invariant") {
    forAll(preconditionsAndCreatePaymentChannelContractTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
      deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, charge: ExecuteContractFunctionTransaction,
      executePayment: ExecuteContractFunctionTransaction, fee: Long, ts: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.create(create.timestamp + 1, Seq(create, charge, executePayment))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe - 3 * fee
        totalPortfolioDiff.effectiveBalance shouldBe - 3 * fee

        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val user = executePayment.proofs.firstCurveProof.explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val contractId = reg.contractId.bytes
        val vsysId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

        //Statevar keys
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val contractTokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        //Statemap keys
        val channelId = DataEntry(Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr, DataType.ShortBytes).bytes
        val balanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val creatorKey = ByteStr(Bytes.concat(contractId.arr,  Array(1.toByte), channelId))
        val creatorPublicKeyKey = ByteStr(Bytes.concat(contractId.arr,  Array(2.toByte), channelId))
        val channelRecipientKey = ByteStr(Bytes.concat(contractId.arr,  Array(3.toByte), channelId))
        val channelCapacityInContractKey = ByteStr(Bytes.concat(contractId.arr,  Array(4.toByte), channelId))
        val executedKey = ByteStr(Bytes.concat(contractId.arr,  Array(5.toByte), channelId))
        val expiredTimeKey = ByteStr(Bytes.concat(contractId.arr,  Array(6.toByte), channelId))
        val channelStatusKey = ByteStr(Bytes.concat(contractId.arr,  Array(7.toByte), channelId))

        val (_, masterTxs) = newState.accountTransactionIds(master, 5, 0)
        masterTxs.size shouldBe 5 // genesis, reg, deposit, create, charge

        //channel info
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractPaymentChannel.contract))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(contractTokenIdKey) shouldEqual Some(DataEntry(vsysId.arr, DataType.TokenId))
        newState.contractInfo(creatorKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(creatorPublicKeyKey) shouldEqual Some(DataEntry(master.publicKey, DataType.PublicKey))
        newState.contractInfo(channelRecipientKey) shouldEqual Some(DataEntry(user.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(expiredTimeKey) shouldEqual Some(DataEntry(Longs.toByteArray(ts + 1000000000000L), DataType.Timestamp))
        newState.contractInfo(channelStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))

        //channel num info
        newState.contractNumInfo(balanceInContractKey) shouldBe 10000L - 200L // deposited - locked
        newState.contractNumInfo(channelCapacityInContractKey) shouldBe 200L // locked
        newState.contractNumInfo(executedKey) shouldBe 50L

        // VSYS balance
        newState.balance(master.toAddress) shouldBe ENOUGH_AMT - 4 * fee - 10000L
        newState.balance(user.toAddress) shouldBe ENOUGH_AMT - fee
        newState.balance(reg.contractId) shouldBe 10000L
      }
    }
  }

  val preconditionsAndPaymentChannelFunctionTest: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    user <- accountGen
    genesis <- genesisPaymentChannelGen(master, ts)
    contract <- channelContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    // Register a payment channel that supports VSYS
    regContract <- registerPaymentChannelGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
    updateTimeData = Seq(create.id.arr, Longs.toByteArray(create.timestamp + 1000000000000L))
    updateTimeType = Seq(DataType.ShortBytes, DataType.Timestamp)
    updateTime <- updateExpiredTimeChannelGen(master, contractId, updateTimeData, updateTimeType, attach, fee, ts)
    executeWithdraw <- executeWithdrawChannelGen(master, contractId, create.id.arr, attach, fee, ts)
    terminateChannel <- terminateChannelGen(master, contractId, create.id.arr, attach, fee, ts)
  } yield (genesis, regContract, depositVSYS, create, executeWithdraw, updateTime, terminateChannel, fee, ts)

  property("execute update expired time successfully") {
    forAll(preconditionsAndPaymentChannelFunctionTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
      deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, _,
      updateTime: ExecuteContractFunctionTransaction, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(reg, deposit, create))),
        TestBlock.create(Seq(updateTime))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("terminate channel successfully") {
    forAll(preconditionsAndPaymentChannelFunctionTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
      deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction,
      _, _, terminate: ExecuteContractFunctionTransaction, _, _) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(reg, deposit, create))),
          TestBlock.create(Seq(terminate))) { blockDiffEi =>
          blockDiffEi shouldBe an[Right[_, _]]
          blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
          blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
          blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
          blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
        }
    }
  }

  property("execute withdraw able to withdraw funds from channel after channel expires") {
    forAll(preconditionsAndPaymentChannelFunctionTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
      deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction,
      executeWithdraw: ExecuteContractFunctionTransaction, _, _, fee: Long, ts: Long) =>
        assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(create.timestamp, Seq(reg, deposit, create))),
          TestBlock.create(create.timestamp + 1000000000001L, Seq(executeWithdraw))) { (blockDiff, newState) =>
          blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
          val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
          totalPortfolioDiff.balance shouldBe - fee
          totalPortfolioDiff.effectiveBalance shouldBe - fee

          val master = reg.proofs.firstCurveProof.explicitGet().publicKey
          val masterBytes = genesis.recipient.bytes.arr
          val contractId = reg.contractId.bytes

          //stateMap keys
          val channelId = DataEntry(Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr, DataType.ShortBytes).bytes
          val balanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
          val channelCapacityInContractKey = ByteStr(Bytes.concat(contractId.arr,  Array(4.toByte), channelId))
          val executedKey = ByteStr(Bytes.concat(contractId.arr,  Array(5.toByte), channelId))

          val (_, masterTxs) = newState.accountTransactionIds(master, 5, 0)
          masterTxs.size shouldBe 5 // genesis, reg, deposit, create, executeWithdraw

          //channel num info
          newState.contractNumInfo(balanceInContractKey) shouldBe 10000L
          newState.contractNumInfo(channelCapacityInContractKey) shouldBe 100L
          newState.contractNumInfo(executedKey) shouldBe 100L

          // VSYS balance
          newState.balance(master.toAddress) shouldBe ENOUGH_AMT - 4 * fee - 10000L

        }
    }
  }

  val preconditionsAndCreatePaymentChannelWithTokenTest: Gen[(GenesisTransaction, GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisPaymentChannelGen(master, ts)
    user <- accountGen
    genesis2 <- genesisPaymentChannelGen(user, ts)
    tContract <- tokenContract
    cContract <- channelContract
    description <- validDescStringGen
    initDataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    regTokenContract <- registerTokenGen(master, tContract, initDataStack, description, fee + 10000000000L, ts)
    tokenContractId = regTokenContract.contractId
    tokenId = tokenIdFromBytes(tokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(tokenId.arr)
    // register a payment channel contract only support regTokenContract's' tokenId
    regContract <- registerPaymentChannelGen(master, cContract, dataStack, description, fee + 10000000000L, ts + 1)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueToken <- issueTokenGen(master, tokenContractId, 100000L, attach, fee, ts + 1)
    // use token contract deposit/withdraw
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositToken <- depositTokenGen(master, tokenContractId, false, depositData, depositType, attach, fee, ts + 2)
    //create payment channel
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts + 3)
    //charge payment channel
    chargeData = Seq(create.id.arr, Longs.toByteArray(100L))
    chargeType = Seq(DataType.ShortBytes, DataType.Amount)
    charge <- chargeChannelGen(master, contractId, chargeData, chargeType, attach, fee, ts + 4)
    //execute payment from channel
    paymentSignatureBytes = EllipticCurveImpl.sign(master, Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr ++ Longs.toByteArray(200L))
    executePaymentData = Seq(create.id.arr, Longs.toByteArray(200L), paymentSignatureBytes)
    executePaymentType = Seq(DataType.ShortBytes, DataType.Amount, DataType.ShortBytes)
    executePayment <- executePaymentChannelGen(user, contractId, executePaymentData, executePaymentType, attach, fee, ts + 5)
    withdrawData = Seq(contractId.bytes.arr, user.toAddress.bytes.arr, Longs.toByteArray(200L))
    withdrawType = Seq(DataType.ContractAccount, DataType.Address, DataType.Amount)
    withdraw <- withdrawTokenGen(user, tokenContractId, false, withdrawData, withdrawType, attach, fee, ts + 6)
  } yield (genesis, genesis2, regContract, regTokenContract, issueToken, depositToken, create, charge, executePayment, withdraw, fee, ts)

  property("Execute payment channel with token doesn't break invariant") {
    forAll(preconditionsAndCreatePaymentChannelWithTokenTest) { case (genesis: GenesisTransaction, genesis2: GenesisTransaction, reg: RegisterContractTransaction,
    reg2: RegisterContractTransaction, issue: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction,
    charge: ExecuteContractFunctionTransaction, executePayment: ExecuteContractFunctionTransaction, withdraw: ExecuteContractFunctionTransaction, fee: Long, ts: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis, genesis2)), TestBlock.create(deposit.timestamp, Seq(reg, reg2, issue, deposit))),
        TestBlock.create(create.timestamp + 1, Seq(create, charge, executePayment, withdraw))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe - 4 * fee
        totalPortfolioDiff.effectiveBalance shouldBe - 4 * fee

        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val user = executePayment.proofs.firstCurveProof.explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val userBytes = genesis2.recipient.bytes.arr
        val contractId = reg.contractId.bytes
        val tokenContractId = reg2.contractId.bytes
        val tokenId = tokenIdFromBytes(tokenContractId.arr, Ints.toByteArray(0)).explicitGet()

        //Statevar keys
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val contractTokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        //Statemap keys
        val channelId = DataEntry(Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr, DataType.ShortBytes).bytes
        val balanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val creatorKey = ByteStr(Bytes.concat(contractId.arr,  Array(1.toByte), channelId))
        val creatorPublicKeyKey = ByteStr(Bytes.concat(contractId.arr,  Array(2.toByte), channelId))
        val channelRecipientKey = ByteStr(Bytes.concat(contractId.arr,  Array(3.toByte), channelId))
        val channelCapacityInContractKey = ByteStr(Bytes.concat(contractId.arr,  Array(4.toByte), channelId))
        val executedKey = ByteStr(Bytes.concat(contractId.arr,  Array(5.toByte), channelId))
        val expiredTimeKey = ByteStr(Bytes.concat(contractId.arr,  Array(6.toByte), channelId))
        val channelStatusKey = ByteStr(Bytes.concat(contractId.arr,  Array(7.toByte), channelId))

        //token balance keys
        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, masterBytes))
        val userBalanceKey = ByteStr(Bytes.concat(tokenId.arr, userBytes))
        val contractBalanceKey = ByteStr(Bytes.concat(tokenId.arr, contractId.arr))

        val (_, masterTxs) = newState.accountTransactionIds(master, 7, 0)
        masterTxs.size shouldBe 7 // genesis, reg1, reg2, issue, deposit, create, charge

        //channel info
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractPaymentChannel.contract))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(contractTokenIdKey) shouldEqual Some(DataEntry(tokenId.arr, DataType.TokenId))
        newState.contractInfo(creatorKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(creatorPublicKeyKey) shouldEqual Some(DataEntry(master.publicKey, DataType.PublicKey))
        newState.contractInfo(channelRecipientKey) shouldEqual Some(DataEntry(user.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(expiredTimeKey) shouldEqual Some(DataEntry(Longs.toByteArray(ts + 1000000000000L), DataType.Timestamp))
        newState.contractInfo(channelStatusKey) shouldEqual Some(DataEntry(Array(1.toByte), DataType.Boolean))

        //channel num info
        newState.contractNumInfo(balanceInContractKey) shouldBe 10000L - 200L // deposited - locked
        newState.contractNumInfo(channelCapacityInContractKey) shouldBe 200L // locked
        newState.contractNumInfo(executedKey) shouldBe 200L

        // VSYS balance
        newState.balance(master.toAddress) shouldBe ENOUGH_AMT - 6 * fee - 2 * 10000000000L
        newState.balance(reg.contractId) shouldBe 0L

        //Token balance
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 100000L - 10000L
        newState.tokenAccountBalance(contractBalanceKey) shouldBe 10000L - 200L
        newState.tokenAccountBalance(userBalanceKey) shouldBe 200L
      }
    }
  }

  val preconditionsAndPaymentChannelWithTokenFunctionTest: Gen[(GenesisTransaction, RegisterContractTransaction,
    RegisterContractTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction,
    ExecuteContractFunctionTransaction, Long, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisPaymentChannelGen(master, ts)
    user <- accountGen
    tContract <- tokenContract
    cContract <- channelContract
    description <- validDescStringGen
    initDataStack: Seq[DataEntry] <- initTokenDataStackGen(100000000L, 100L, "init")
    regTokenContract <- registerTokenGen(master, tContract, initDataStack, description, fee + 10000000000L, ts)
    tokenContractId = regTokenContract.contractId
    tokenId = tokenIdFromBytes(tokenContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(tokenId.arr)
    // register a payment channel contract only support regTokenContract's' tokenId
    regContract <- registerPaymentChannelGen(master, cContract, dataStack, description, fee + 10000000000L, ts + 1)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    issueToken <- issueTokenGen(master, tokenContractId, 100000L, attach, fee, ts + 1)
    // use token contract deposit/withdraw
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositToken <- depositTokenGen(master, tokenContractId, false, depositData, depositType, attach, fee, ts + 2)
    //create payment channel
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts + 3)
    //update expired time
    updateTimeData = Seq(create.id.arr, Longs.toByteArray(create.timestamp + 1000000000000L))
    updateTimeType = Seq(DataType.ShortBytes, DataType.Timestamp)
    updateTime <- updateExpiredTimeChannelGen(master, contractId, updateTimeData, updateTimeType, attach, fee, ts + 4)
    //execute withdraw
    executeWithdraw <- executeWithdrawChannelGen(master, contractId, create.id.arr, attach, fee, ts + 5)
    //terminate channel
    terminateChannel <- terminateChannelGen(master, contractId, create.id.arr, attach, fee, ts + 6)

  } yield (genesis, regContract, regTokenContract, issueToken, depositToken, create, updateTime, executeWithdraw, terminateChannel, fee, ts)

  property("execute update expired time in token payment channel successfully") {
    forAll(preconditionsAndPaymentChannelWithTokenFunctionTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2: RegisterContractTransaction, issue: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction,
    create: ExecuteContractFunctionTransaction, updateTime: ExecuteContractFunctionTransaction, _, _, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(reg, reg2, issue, deposit, create))),
        TestBlock.create(Seq(updateTime))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("terminate channel in token payment channel successfully") {
    forAll(preconditionsAndPaymentChannelWithTokenFunctionTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2: RegisterContractTransaction, issue: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction,
    create: ExecuteContractFunctionTransaction, _, _, terminate: ExecuteContractFunctionTransaction, _, _) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(reg, reg2, issue, deposit, create))),
        TestBlock.create(Seq(terminate))) { blockDiffEi =>
        blockDiffEi shouldBe an[Right[_, _]]
        blockDiffEi.explicitGet().txsDiff.contractDB.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.contractNumDB.isEmpty shouldBe true
        blockDiffEi.explicitGet().txsDiff.portfolios.isEmpty shouldBe false
        blockDiffEi.explicitGet().txsDiff.txStatus shouldBe TransactionStatus.Success
      }
    }
  }

  property("execute withdraw able to withdraw tokens from channel after channel expires") {
    forAll(preconditionsAndPaymentChannelWithTokenFunctionTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
    reg2: RegisterContractTransaction, issue: ExecuteContractFunctionTransaction, deposit: ExecuteContractFunctionTransaction,
    create: ExecuteContractFunctionTransaction, _, executeWithdraw: ExecuteContractFunctionTransaction, _, fee: Long, _) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(create.timestamp, Seq(reg, reg2, issue, deposit, create))),
        TestBlock.create(create.timestamp + 1000000000001L, Seq(executeWithdraw))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe - fee
        totalPortfolioDiff.effectiveBalance shouldBe - fee

        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val contractId = reg.contractId.bytes
        val tokenContractId = reg2.contractId.bytes
        val tokenId = tokenIdFromBytes(tokenContractId.arr, Ints.toByteArray(0)).explicitGet()

        //stateMap keys
        val channelId = DataEntry(Shorts.toByteArray(create.id.arr.length.toShort) ++ create.id.arr, DataType.ShortBytes).bytes
        val balanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val channelCapacityInContractKey = ByteStr(Bytes.concat(contractId.arr,  Array(4.toByte), channelId))
        val executedKey = ByteStr(Bytes.concat(contractId.arr,  Array(5.toByte), channelId))

        val (_, masterTxs) = newState.accountTransactionIds(master, 7, 0)
        masterTxs.size shouldBe 7 // genesis, reg, reg2, issue, deposit, create, executeWithdraw

        //channel num info
        newState.contractNumInfo(balanceInContractKey) shouldBe 10000L
        newState.contractNumInfo(channelCapacityInContractKey) shouldBe 100L
        newState.contractNumInfo(executedKey) shouldBe 100L

        // token balance keys
        val masterBalanceKey = ByteStr(Bytes.concat(tokenId.arr, masterBytes))
        val contractBalanceKey = ByteStr(Bytes.concat(tokenId.arr, contractId.arr))

        // VSYS balance
        newState.balance(master.toAddress) shouldBe ENOUGH_AMT - 6 * fee - 2 * 10000000000L

        // Token balance
        newState.tokenAccountBalance(masterBalanceKey) shouldBe 100000L - 10000L
        newState.tokenAccountBalance(contractBalanceKey) shouldBe 10000L
      }
    }
  }
}
