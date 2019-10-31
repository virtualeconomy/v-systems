package vsys.blockchain.contract

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.{Address, ContractAccount}

trait DataStack {

  def initDataStackGen(amount: Long, unity: Long, desc: String): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    unit <- Gen.const(DataEntry(Longs.toByteArray(unity), DataType.Amount))
    shortText <- Gen.const(DataEntry.create(desc.getBytes(), DataType.ShortText).right.get)
  } yield Seq(max, unit, shortText)

  def supersedeDataStackGen(newIssuer: Address): Gen[Seq[DataEntry]] = for {
    iss <- Gen.const(DataEntry(newIssuer.bytes.arr, DataType.Address))
  } yield Seq(iss)

  def splitDataStackGen(newUnity: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    unit <- Gen.const(DataEntry(Longs.toByteArray(newUnity), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(unit, index)

  def splitDataStackGen(newUnity: Long): Gen[Seq[DataEntry]] = for {
    unit <- Gen.const(DataEntry(Longs.toByteArray(newUnity), DataType.Amount))
  } yield Seq(unit)

  def destroyDataStackGen(amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(am, index)

  def destroyDataStackGen(amount: Long): Gen[Seq[DataEntry]] = for {
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(am)

  def issueDataStackGen(amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(max, index)

  def issueDataStackGen(amount: Long): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(max)

  def sendDataStackGen(recipient: Address, amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(reci, am, index)

  def sendDataStackGen(recipient: Address, amount: Long): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(reci, am)

  def transferDataStackGen(sender: Address, recipient: Address, amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    se <- Gen.const(DataEntry(sender.bytes.arr, DataType.Address))
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(se, reci, am, index)

  def transferDataStackGen(sender: Address, recipient: Address, amount: Long): Gen[Seq[DataEntry]] = for {
    se <- Gen.const(DataEntry(sender.bytes.arr, DataType.Address))
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(se, reci, am)

  def transferDataStackGen(sender: Address, recipient: ContractAccount, amount: Long): Gen[Seq[DataEntry]] = for {
    se <- Gen.const(DataEntry(sender.bytes.arr, DataType.Address))
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.ContractAccount))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(se, reci, am)

  def transferDataStackGen(sender: ContractAccount, recipient: Address, amount: Long): Gen[Seq[DataEntry]] = for {
    se <- Gen.const(DataEntry(sender.bytes.arr, DataType.ContractAccount))
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(se, reci, am)

  def depositDataStackGen(sender: Address, smartContract: Address, amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    se <- Gen.const(DataEntry(sender.bytes.arr, DataType.Address))
    sc <- Gen.const(DataEntry(smartContract.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(se, sc, am, index)

  def depositDataStackGen(sender: Address, smartContract: Address, amount: Long): Gen[Seq[DataEntry]] = for {
    se <- Gen.const(DataEntry(sender.bytes.arr, DataType.Address))
    sc <- Gen.const(DataEntry(smartContract.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(se, sc, am)

  def withdrawDataStackGen(smartContract: Address, recipient: Address, amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    sc <- Gen.const(DataEntry(smartContract.bytes.arr, DataType.Address))
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(sc, reci, am, index)

  def withdrawDataStackGen(smartContract: Address, recipient: Address, amount: Long): Gen[Seq[DataEntry]] = for {
    sc <- Gen.const(DataEntry(smartContract.bytes.arr, DataType.Address))
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(sc, reci, am)

  def totalSupplyDataStackGen(tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(index)

  def maxSupplyDataStackGen(tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(index)

  def balanceOfDataStackGen(account: Address, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    acc <- Gen.const(DataEntry(account.bytes.arr, DataType.Address))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(acc, index)

  def balanceOfDataStackGen(account: Address): Gen[Seq[DataEntry]] = for {
    acc <- Gen.const(DataEntry(account.bytes.arr, DataType.Address))
  } yield Seq(acc)

  def emptyDataStackGen(): Gen[Seq[DataEntry]] = Seq()
}

object DataStack {

  // case for non tokenIndex case
  object initInput {
    val maxIndex: Byte = 0
    val unityIndex: Byte = 1
    val shortTextIndex: Byte = 2
    val issuerLoadIndex: Byte = 3
  }

  object supersedeInput {
    val newIssuerIndex: Byte = 0
    val maker: Byte = 1
  }

  object splitInput {
    val newUnityIndex: Byte = 0
    val issuerGetIndex: Byte = 1
  }

  object destroyInput {
    val destroyAmountIndex: Byte = 0
    val issuerGetIndex: Byte = 1
  }

  object issueInput {
    val amountIndex: Byte = 0
    val issuerGetIndex: Byte = 1
  }

  object sendInput {
    val recipientIndex: Byte = 0
    val amountIndex: Byte = 1
    val senderIndex: Byte = 2
  }

  object transferInput {
    val senderIndex: Byte = 0
    val recipientIndex: Byte = 1
    val amountIndex: Byte = 2
  }

  object depositInput {
    val senderIndex: Byte = 0
    val smartContractIndex: Byte = 1
    val amountIndex: Byte = 2
  }

  object withdrawInput {
    val smartContractIndex: Byte = 0
    val recipientIndex: Byte = 1
    val amountIndex: Byte = 2
  }

  object balanceOfInput {
    val accountIndex: Byte = 0
  }

  // TODO
  // with tokenIndex cases
}
