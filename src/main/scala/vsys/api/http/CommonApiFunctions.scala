package vsys.api.http

import akka.http.scaladsl.server.Directive1
import vsys.blockchain.block.Block
import vsys.blockchain.history.History
import vsys.blockchain.state.ByteStr
import vsys.blockchain.transaction.TransactionParser


trait CommonApiFunctions { this: ApiRoute =>
  protected[api] def withBlock(history: History, encodedSignature: String): Directive1[Block] =
  if (encodedSignature.length > TransactionParser.SignatureStringLength) complete(InvalidSignature) else {
    ByteStr.decodeBase58(encodedSignature).toOption.toRight(InvalidSignature)
        .flatMap(s => history.blockById(s).toRight(BlockNotExists)) match {
      case Right(b) => provide(b)
      case Left(e) => complete(e)
    }
  }
}
