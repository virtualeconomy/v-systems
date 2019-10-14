package vsys.settings

import com.typesafe.config.Config
import scala.util.Try
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.account.Address
import vsys.blockchain.transaction.ProcessedTransaction
import vsys.utils.{ScorexLogging, TransactionHelper}

trait WebhookEventRules extends Product with Serializable with ScorexLogging {
  val value: Any
  def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean
}

trait RuleConfigReader {
  val field: String
  private val path = "rules"
  lazy val valPath = path + "." + field

  def fromConfig(config: Config): Option[WebhookEventRules]

  def read[T: ValueReader](config: Config): Option[T] = {
    Try(config.as[T](valPath)).toOption
  }
}

case class AfterHeight(value: Long) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = height >= value
}

object AfterHeight extends RuleConfigReader {
  override val field = "afterHeight"

  override def fromConfig(config: Config): Option[AfterHeight] = {
    read[Long](config).map(AfterHeight(_))
  }
}

case class AfterTime(value: Long) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = blockTime >= value
}

object AfterTime extends RuleConfigReader {
  override val field = "afterTime"

  override def fromConfig(config: Config): Option[AfterTime] = {
    read[Long](config).map(AfterTime(_))
  }
}

case class WithTxs(value: Boolean) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = value
}

object WithTxs extends RuleConfigReader {
  override val field = "withTxs"

  override def fromConfig(config: Config): Option[WithTxs] = {
    read[Boolean](config).map(WithTxs(_))
  }
}

case class WithMintingTxs(value: Boolean) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    tx.transaction.transactionType != TransactionType.MintingTransaction | value
  }
}

object WithMintingTxs extends RuleConfigReader {
  override val field = "withMintingTxs"

  override def fromConfig(config: Config): Option[WithMintingTxs] = {
    read[Boolean](config).map(WithMintingTxs(_))
  }
}

case class RelatedAccs(value: Seq[String]) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    val addrInTx = accs.toSeq.map(_.toString)
    if(value.nonEmpty) {
      value.intersect(addrInTx).nonEmpty
    } else {
      addrInTx.isEmpty
    }
  }
}

object RelatedAccs extends RuleConfigReader {
  override val field = "relatedAccount"

  override def fromConfig(config: Config): Option[RelatedAccs] = {
    read[Seq[String]](config).map(RelatedAccs(_))
  }
}

case class IncludeTypes(value: Seq[Int]) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    value.contains(tx.transaction.transactionType.id)
  }
}

object IncludeTypes extends RuleConfigReader {
  override val field = "includeTypes"

  override def fromConfig(config: Config): Option[IncludeTypes] = {
    read[Seq[Int]](config).map(IncludeTypes(_))
  }
}

case class ExcludeTypes(value: Seq[Int]) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    !value.contains(tx.transaction.transactionType.id)
  }
}

object ExcludeTypes extends RuleConfigReader {
  override val field = "excludeTypes"

  override def fromConfig(config: Config): Option[ExcludeTypes] = {
    read[Seq[Int]](config).map(ExcludeTypes(_))
  }
}

case class Amount(value: Long, gteVal: Long, ltVal: Long, lteVal: Long, withFeeVal: Boolean) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    val (amt, fee) = TransactionHelper.extractAmtFee(tx)

    if (withFeeVal) {
      validateVal(amt + fee, value, gteVal, ltVal, lteVal)
    } else {
      validateVal(amt, value, gteVal, ltVal, lteVal)
    }
  }

  private def validateVal(amt: Long, gtVal: Long, gteVal: Long, ltVal: Long, lteVal: Long): Boolean = {
    amt > gtVal && amt >= gteVal && amt < ltVal && amt <= lteVal
  }
}

object Amount extends RuleConfigReader {
  override val field = "amount"

  override def fromConfig(config: Config): Option[Amount] = {
    val gtVal = getVal[Long](config, "gt").getOrElse(0L)
    val gteVal = getVal[Long](config, "gte").getOrElse(0L)
    val ltVal = getVal[Long](config, "lt").getOrElse(Long.MaxValue)
    val lteVal = getVal[Long](config, "lte").getOrElse(Long.MaxValue)
    val withFeeVal = getVal[Boolean](config, "withFee").getOrElse(false)

    if (config.hasPath("rules.amount")) Some(Amount(gtVal, gteVal, ltVal, lteVal, withFeeVal)) else None
  }

  private def getVal[T: ValueReader](config: Config, r: String): Option[T] = {
    Try(config.as[T]("rules." + field + "." + r)).toOption
  }
}

case class WithTxsOfTypes(value: Seq[Int]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    value.contains(tx.transaction.transactionType.txType)
  }
}

object WithTxsOfTypes extends RuleConfigReader {
  override val field = "withTxsOfTypes"

  override def fromConfig(config: Config): Option[WithTxsOfTypes] = {
    read[Seq[Int]](config).map(WithTxsOfTypes(_))
  }
}

case class WithTxsOfAccs(value: Seq[String]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    accs.foldLeft(false) {case (accum, acc) => value.contains(acc.toString) || accum}
  }
}

object WithTxsOfAccs extends RuleConfigReader {
  override val field = "withTxsOfAccs"

  override def fromConfig(config: Config): Option[WithTxsOfAccs] = {
    read[Seq[String]](config).map(WithTxsOfAccs(_))
  }
}

case class WithStateOfAccs(value: Seq[String]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = ???
}

object WithStateOfAccs extends RuleConfigReader {
  override val field = "withStateOfAccs"

  override def fromConfig(config: Config): Option[WithStateOfAccs] = {
    read[Seq[String]](config).map(WithStateOfAccs(_))
  }
}