package vsys.state.opcdiffs

import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType}

import scala.util.{Left, Right}

object CDBOpcDiff {

  //val MaxNum: DataEntry = DataEntry(Array(1000000000L.toByte), DataType.Amount)

  // set function
  def set(contractID: DataEntry, issuer: DataEntry, index: DataEntry): Either[ValidationError, OpcDiff] = {
    if ((contractID.dataType != DataType.Address) || (issuer.dataType != DataType.Address)) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      Right(OpcDiff(contractDB = Map(ByteStr(contractID.data) -> issuer.data),
        contractTokens = Map(ByteStr(contractID.data) -> 1)
     ))
    }
  }
}

