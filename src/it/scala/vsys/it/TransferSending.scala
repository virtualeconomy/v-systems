package vsys.it

import java.security.SecureRandom

import vsys.it.TransferSending.Req
import vsys.it.api.NodeApi.Transaction
import vsys.utils.VSYSSecureRandom

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object TransferSending {
  case class Req(source: String, targetAddress: String, amount: Long, fee: Long)
}

trait TransferSending {
  def nodes: Seq[Node]

  def generateRequests(n: Int, balances: Map[String, Long]): Seq[Req] = {
    val fee = 100000
    val addresses = nodes.map(_.address)
    val sourceAndDest = (1 to n).map { _ =>
      val Seq(src, dest) = VSYSSecureRandom.shuffle(addresses).take(2)
      (src, dest)
    }
    val requests = sourceAndDest.foldLeft(List.empty[Req]) {
      case (rs, (src, dest)) =>
        val transferAmount = (1e-8 + new SecureRandom().nextDouble() * 1e-8 * balances(src)).toLong
        rs :+ Req(src, dest, transferAmount, fee)
    }

    requests
  }

  def balanceForNode(n: Node): Future[(String, Long)] = n.balance(n.address).map(b => b.address -> b.balance)

  def makeTransfer(r: Req): Future[Transaction] = {
    val addressToNode = nodes.map(n => n.address -> n).toMap
    addressToNode(r.source).payment(r.source, r.targetAddress, r.amount, r.fee, 100.toShort)
  }

  def processRequests(requests: Seq[Req]): Future[Unit] = if (requests.isEmpty) {
    Future.successful(())
  } else {
    makeTransfer(requests.head).flatMap(_ => processRequests(requests.tail))
  }

}
