package vsys.state.opcdiffs

import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.{DataEntry, DataType, ExecutionContext}

import scala.util.{Left, Right}

object CDBOpcDiff {

  def set(context: ExecutionContext)(stateVarIssuer: Array[Byte],
                                     issuer: DataEntry): Either[ValidationError, OpcDiff] = {
    if (stateVarIssuer.length != 2 || DataType.fromByte(stateVarIssuer(1)).get != DataType.Address) {
      Left(GenericError(s"wrong stateVariable $stateVarIssuer"))
    } else if (issuer.dataType != DataType.Address) {
      Left(GenericError("Input contains invalid dataType"))
    } else {
      Right(OpcDiff(contractDB = Map(ByteStr(context.contractId.bytes.arr
        ++ Array(stateVarIssuer(0))) -> issuer.bytes)
     ))
    }
  }

  object CDBType extends Enumeration {
    val SetCDB = Value(1)
  }

}
