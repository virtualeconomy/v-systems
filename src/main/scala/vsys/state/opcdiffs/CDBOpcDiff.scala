package vsys.state.opcdiffs

import com.google.common.primitives.Bytes
import com.wavesplatform.state2._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.GenericError
import vsys.contract.ExecutionContext
import scala.util.{Left, Right}

object CDBOpcDiff {
  def set(contractContext: ExecutionContext): Either[ValidationError, OpcDiff] = {
    if (contractContext.height <= 0) {
      Left(GenericError(s"Height ${contractContext.height} is smaller than 0"))
    } else {
      Right(OpcDiff(contractDB = Map(contractContext.contractId.bytes -> contractContext.signers.head.publicKey,
        ByteStr(Bytes.concat(contractContext.contractId.bytes.arr, Array("desc".toByte))) -> contractContext.description)
     ))
    }
  }
}