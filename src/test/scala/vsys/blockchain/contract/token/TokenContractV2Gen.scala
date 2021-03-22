package vsys.blockchain.contract.token

import com.google.common.primitives.{Ints, Longs}
import org.scalacheck.Gen
import vsys.account.Address
import vsys.blockchain.contract.{Contract, ContractTokenV2, DataEntry, DataType}

trait TokenContractV2Gen {

  def initTokenV2DataStackGen(amount: Long, unity: Long, desc: String): Gen[Seq[DataEntry]] = for {
    max <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    unit <- Gen.const(DataEntry(Longs.toByteArray(unity), DataType.Amount))
    shortText <- Gen.const(DataEntry.create(desc.getBytes(), DataType.ShortText).right.get)
  } yield Seq(max, unit, shortText)

  def amountDataStackGen(amount: Long): Gen[Seq[DataEntry]] = for {
    res <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(res)

  def addressDataStackGen(address: Address): Gen[Seq[DataEntry]] = for {
    add <- Gen.const(DataEntry(address.bytes.arr, DataType.Address))
  } yield Seq(add)

  def sendDataStackGen(recipient: Address, amount: Long, tokenIndex: Int): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
    index <- Gen.const(DataEntry(Ints.toByteArray(tokenIndex), DataType.Int32))
  } yield Seq(reci, am, index)

  def sendDataStackGen(recipient: Address, amount: Long): Gen[Seq[DataEntry]] = for {
    reci <- Gen.const(DataEntry(recipient.bytes.arr, DataType.Address))
    am <- Gen.const(DataEntry(Longs.toByteArray(amount), DataType.Amount))
  } yield Seq(reci, am)

  def tokenContractV2Gen(white: Boolean): Gen[Contract] =
    if (white) ContractTokenV2.contractTokenWhiteList
    else ContractTokenV2.contractTokenBlackList

}
