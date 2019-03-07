package vsys.state.opcdiffs


import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}

import scala.util.{Left, Right}

object CDBOpcDiff {

  //val MaxNum: DataEntry = DataEntry(Array(1000000000L.toByte), DataType.Amount)

  private def validateType(dataType: Seq[DataType.Value], dataEntry: Seq[DataEntry]): Either[ValidationError, Boolean] = {
    dataType.zip(dataEntry).foreach( x => {
      if (x._1 != x._2.dataType)
        Left(GenericError("The type in DataEntry is not consistent with the given DataType"))
    })
    Right(true)
  }

  // set function: to set issuer in contractDB [contractID -> issuer]
  def set(dataType: Seq[DataType.Value], dataEntry: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    //if (dataType.length > 2)
      //Left(GenericError("Too many inputs for issuer"))

    if (validateType(dataType, dataEntry).isRight)
      Right(OpcDiff(contractDB = Map(ByteStr(dataEntry(1).data) -> dataEntry(3).data),
        contractTokens = Map(ByteStr(dataEntry(1).data) -> 1)
      ))
    else
      Left(GenericError("Errors occurred"))
  }

  def get(dataType: Seq[DataType.Value], dataEntry: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    if (validateType(dataType, dataEntry).isRight)
      Right(OpcDiff(tokenDB = Map(ByteStr(dataEntry(2).data) -> dataEntry(6).data)
      ))
    else
      Left(GenericError("Errors occurred"))
  }

  // deposit function: to deposit an amount a money in tokenAccountBalance [tokenID + "total" -> amount], [tokenID + address -> amount]
  def deposit(dataType: Seq[DataType.Value], dataEntry: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    //if (dataType.length > 4)
      //Left(GenericError("Too many inputs for issuer"))

    if (validateType(dataType, dataEntry).isRight) {

      // amount <= 0 is invalid
      if (Longs.fromByteArray(dataEntry(11).data) <= 0)
        Left(GenericError("Amount should be larger than 0"))

      // amount + total > max is invalid
      if (Longs.fromByteArray(dataEntry(11).data) + Longs.fromByteArray(dataEntry(7).data) > Longs.fromByteArray(dataEntry(6).data))
        Left(GenericError("New total is larger than max"))

      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(dataEntry(2).data, Array("total".toByte))) -> Longs.fromByteArray(dataEntry(11).data),
        ByteStr(Bytes.concat(dataEntry(2).data,dataEntry(3).data)) -> Longs.fromByteArray(dataEntry(11).data))
      ))
    }
    else
        Left(GenericError("Errors occurred"))
  }

  def withdraw(dataType: Seq[DataType.Value], dataEntry: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {

    if (validateType(dataType, dataEntry).isRight) {

      // amount <= 0 is invalid
      if (Longs.fromByteArray(dataEntry(11).data) <= 0)
        Left(GenericError("Amount should be larger than 0"))

      // amount > issuer balance || amount > total is invalid
      if (Longs.fromByteArray(dataEntry(11).data) > Longs.fromByteArray(dataEntry(8).data) || Longs.fromByteArray(dataEntry(11).data) > Longs.fromByteArray(dataEntry(7).data))
        Left(GenericError("Input amount is larger than issuer's balance or total"))

      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(dataEntry(2).data, Array("total".toByte))) -> -Longs.fromByteArray(dataEntry(11).data),
        ByteStr(Bytes.concat(dataEntry(2).data,dataEntry(3).data)) -> -Longs.fromByteArray(dataEntry(11).data))
      ))
    }
    else
      Left(GenericError("Errors occurred"))
  }

  def transfer(dataType: Seq[DataType.Value], dataEntry: Seq[DataEntry]): Either[ValidationError, OpcDiff] = {
    if (validateType(dataType, dataEntry).isRight) {

      // amount <= 0 is invalid
      if (Longs.fromByteArray(dataEntry(11).data) <= 0)
        Left(GenericError("Amount should be larger than 0"))

      // amount > balance is invalid
      if (Longs.fromByteArray(dataEntry(11).data) > Longs.fromByteArray(dataEntry(8).data))
        Left(GenericError("Amount is larger than balance"))

      Right(OpcDiff(tokenAccountBalance = Map(ByteStr(Bytes.concat(dataEntry(2).data,dataEntry(4).data)) -> -Longs.fromByteArray(dataEntry(11).data),
        ByteStr(Bytes.concat(dataEntry(2).data,dataEntry(5).data)) -> Longs.fromByteArray(dataEntry(11).data))
      ))
    }
    else
      Left(GenericError("Errors occurred"))
  }

}

