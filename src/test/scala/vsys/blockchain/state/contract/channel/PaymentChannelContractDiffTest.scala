package vsys.blockchain.state.contract.channel

import cats.Monoid
import com.google.common.primitives.{Bytes, Ints, Longs}
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

  val preconditionsAndChannelContractTest: Gen[(GenesisTransaction, RegisterContractTransaction,
    ExecuteContractFunctionTransaction, ExecuteContractFunctionTransaction, Long)] = for {
    (master, ts, fee) <- ContractGenHelper.basicContractTestGen()
    genesis <- genesisPaymentChannelGen(master, ts)
    contract <- channelContract
    description <- validDescStringGen
    sysTokenId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()
    dataStack <- initPaymentChannelContractDataStackGen(sysTokenId.arr)
    // Register a payment channel that supports VSYS
    regContract <- registerPaymentChannelGen(master, contract, dataStack, description, fee, ts)
    contractId = regContract.contractId
    attach <- genBoundedString(2, ExecuteContractFunctionTransaction.MaxDescriptionSize)
    user <- accountGen
    depositData = Seq(master.toAddress.bytes.arr, contractId.bytes.arr, Longs.toByteArray(10000L))
    depositType = Seq(DataType.Address, DataType.ContractAccount, DataType.Amount)
    depositVSYS <- depositVSYSGen(master, depositData, depositType, attach, fee, ts + 1)
    createData = Seq(user.toAddress.bytes.arr, Longs.toByteArray(100L), Longs.toByteArray(ts + 1000000000000L))
    createType = Seq(DataType.Address, DataType.Amount, DataType.Timestamp)
    create <- createChannelGen(master, contractId, createData, createType, attach, fee, ts)
  } yield (genesis, regContract, depositVSYS, create, fee)

  property("Execute payment channel doesn't break invariant") {
    forAll(preconditionsAndChannelContractTest) { case (genesis: GenesisTransaction, reg: RegisterContractTransaction,
      deposit: ExecuteContractFunctionTransaction, create: ExecuteContractFunctionTransaction, fee: Long) =>
      assertDiffAndStateCorrectBlockTime(Seq(TestBlock.create(genesis.timestamp, Seq(genesis)), TestBlock.create(deposit.timestamp, Seq(reg, deposit))),
        TestBlock.create(create.timestamp + 1, Seq(create))) { (blockDiff, newState) =>
        blockDiff.txsDiff.txStatus shouldBe TransactionStatus.Success
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe -fee
        totalPortfolioDiff.effectiveBalance shouldBe -fee

        val master = reg.proofs.firstCurveProof.explicitGet().publicKey
        val masterBytes = genesis.recipient.bytes.arr
        val contractId = reg.contractId.bytes
        val vsysId = tokenIdFromBytes(ContractAccount.systemContractId.bytes.arr, Ints.toByteArray(0)).explicitGet()

        //Statevar keys
        val makerKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte)))
        val contractTokenIdKey = ByteStr(Bytes.concat(contractId.arr, Array(1.toByte)))

        //Statemap keys
        val masterBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr, Array(0.toByte), DataEntry(masterBytes, DataType.Address).bytes))
        val creatorKey = ByteStr(Bytes.concat(contractId.arr,  Array(1.toByte), DataEntry(create.id.arr, DataType.ShortBytes).bytes))
        val creatorPublicKeyKey = ByteStr(Bytes.concat(contractId.arr,  Array(2.toByte), DataEntry(create.id.arr, DataType.ShortBytes).bytes))
        val recipientKey = ByteStr(Bytes.concat(contractId.arr,  Array(3.toByte), DataEntry(create.id.arr, DataType.ShortBytes).bytes))
        val channelBalanceInContractKey = ByteStr(Bytes.concat(contractId.arr,  Array(4.toByte), DataEntry(create.id.arr, DataType.ShortBytes).bytes))
        val executedKey = ByteStr(Bytes.concat(contractId.arr,  Array(5.toByte), DataEntry(create.id.arr, DataType.ShortBytes).bytes))
        val expiredTimeKey = ByteStr(Bytes.concat(contractId.arr,  Array(6.toByte), DataEntry(create.id.arr, DataType.ShortBytes).bytes))
        val channelStatusKey = ByteStr(Bytes.concat(contractId.arr,  Array(7.toByte), DataEntry(create.id.arr, DataType.ShortBytes).bytes))

        val (_, masterTxs) = newState.accountTransactionIds(master, 4, 0)
        masterTxs.size shouldBe 4 // genesis, reg, deposit, create

        //channel statevar info
        newState.contractContent(contractId) shouldEqual Some((2, reg.id, ContractPaymentChannel.contract))
        newState.contractInfo(makerKey) shouldEqual Some(DataEntry(master.toAddress.bytes.arr, DataType.Address))
        newState.contractInfo(contractTokenIdKey) shouldEqual Some(DataEntry(vsysId.arr, DataType.TokenId))

        //channel statemap info
        newState.contractNumInfo(masterBalanceInContractKey) shouldBe 10000L - 100L // deposited - locked
        newState.contractNumInfo(channelBalanceInContractKey) shouldBe 100L // locked

        // VSYS balance
        newState.balance(master.toAddress)shouldBe ENOUGH_AMT - 3 * fee - 10000L
        newState.balance(reg.contractId) shouldBe 10000L
      }
    }
  }
}
