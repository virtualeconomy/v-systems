package vsys.it

import vsys.it.util._
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import vsys.blockchain.transaction.TransactionParser.TransactionType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


trait IntegrationSuiteWithThreeAddresses extends FunSuite with BeforeAndAfterAll with Matchers with ScalaFutures
  with IntegrationPatience with RecoverMethods with RequestErrorAssert with IntegrationTestsScheme {

  def allNodes: Seq[Node]

  def notMiner: Node

  protected val sender: Node = notMiner
  private val richAddress = sender.address

  protected val defaultBalance: Long = 100.vsys

  protected lazy val firstAddress: String = Await.result(sender.createAddress, 1.minutes)
  protected lazy val secondAddress: String = Await.result(sender.createAddress, 1.minutes)
  protected lazy val thirdAddress: String = Await.result(sender.createAddress, 1.minutes)

  protected def assertBalances(acc: String, balance: Long, effectiveBalance: Long): Future[Unit] = {
    for {
      newBalance <- sender.balance(acc).map(_.balance)
      newEffectiveBalance <- sender.effectiveBalance(acc).map(_.balance)
    } yield {
      newEffectiveBalance shouldBe effectiveBalance
      newBalance shouldBe balance
    }
  }

  override def beforeAll(): Unit = {
    super.beforeAll()

    def waitForTxsToReachAllNodes(txIds: Seq[String]): Future[_] = {
      val txNodePairs = for {
        txId <- txIds
        node <- allNodes
      } yield (node, txId)
      Future.traverse(txNodePairs) { case (node, tx) => node.waitForTransaction(tx) }
    }

    def makeTransfers: Future[Seq[String]] = Future.sequence(Seq(
      sender.payment(richAddress, firstAddress, defaultBalance, sender.fee(TransactionType.PaymentTransaction), sender.feeScale),
      sender.payment(richAddress, secondAddress, defaultBalance, sender.fee(TransactionType.PaymentTransaction), sender.feeScale),
      sender.payment(richAddress, thirdAddress, defaultBalance, sender.fee(TransactionType.PaymentTransaction), sender.feeScale))).map(_.map(_.id))

    val correctStartBalancesFuture = for {
      txs <- makeTransfers

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))

      _ <- waitForTxsToReachAllNodes(txs)

      _ <- Future.sequence(Seq(firstAddress, secondAddress, thirdAddress).map(address => assertBalances(address, defaultBalance, defaultBalance)))
    } yield succeed

    Await.result(correctStartBalancesFuture, 2.minutes)
  }
}
