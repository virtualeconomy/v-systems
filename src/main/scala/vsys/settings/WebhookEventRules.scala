package vsys.settings

import com.typesafe.config.Config
import scala.util.Try
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import vsys.blockchain.transaction.TransactionParser.TransactionType
import vsys.utils.TransactionHelper
import vsys.account.Address
import vsys.blockchain.transaction.ProcessedTransaction

trait WebhookEventRules extends Product with Serializable {
  val value: Any
  def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean
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
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    height >= value
  }
}

object AfterHeight extends RuleConfigReader {
  override val field = "afterHeight"

  override def fromConfig(config: Config): Option[AfterHeight] = {
    read[Long](config).map(AfterHeight(_))
  }
}

case class AfterTime(value: Long) extends WebhookEventRules {
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    tx.transaction.timestamp >= value
  }
}

object AfterTime extends RuleConfigReader {
  override val field = "afterTime"

  override def fromConfig(config: Config): Option[AfterTime] = {
    read[Long](config).map(AfterTime(_))
  }
}

case class WithTxs(value: Boolean) extends WebhookEventRules {
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = value
}

object WithTxs extends RuleConfigReader {
  override val field = "withTxs"

  override def fromConfig(config: Config): Option[WithTxs] = {
    read[Boolean](config).map(WithTxs(_))
  }
}

case class WithMintingTxs(value: Boolean) extends WebhookEventRules {
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
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
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
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
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
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
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    !value.contains(tx.transaction.transactionType.id)
  }
}

object ExcludeTypes extends RuleConfigReader {
  override val field = "excludeTypes"

  override def fromConfig(config: Config): Option[ExcludeTypes] = {
    read[Seq[Int]](config).map(ExcludeTypes(_))
  }
}

case class Amount(value: Seq[WebhookEventRules]) extends WebhookEventRules {
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    val (amt, fee) = TransactionHelper.extractAmtFee(tx)
    val feeRule = value.filter(_.isInstanceOf[AmtWithFee])

    if (feeRule.nonEmpty && feeRule(0).applyRule(height, tx, accs)) {
      // withFee
      value.filterNot(_.isInstanceOf[AmtWithFee]).foldLeft(true)((accum, rule) =>
        rule.applyRule(amt+fee, tx, accs) && accum)
    } else {
      // not withFee
      value.filterNot(_.isInstanceOf[AmtWithFee]).foldLeft(true)((accum, rule) =>
        rule.applyRule(amt, tx, accs) && accum)
    }
  }
}

case class AmtGT(value: Long) extends WebhookEventRules {
  override def applyRule(valueIn: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    valueIn > value
  }
}

object AmtGT extends RuleConfigReader {
  override val field = "amount.gt"

  override def fromConfig(config: Config): Option[AmtGT] = {
    read[Long](config).map(AmtGT(_))
  }
}

case class AmtLT(value: Long) extends WebhookEventRules {
  override def applyRule(valueIn: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    valueIn < value
  }
}

object AmtLT extends RuleConfigReader {
  override val field = "amount.lt"

  override def fromConfig(config: Config): Option[AmtLT] = {
    read[Long](config).map(AmtLT(_))
  }
}

case class AmtGTE(value: Long) extends WebhookEventRules {
  override def applyRule(valueIn: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    valueIn >= value
  }
}

object AmtGTE extends RuleConfigReader {
  override val field = "amount.gte"

  override def fromConfig(config: Config): Option[AmtGTE] = {
    read[Long](config).map(AmtGTE(_))
  }
}

case class AmtLTE(value: Long) extends WebhookEventRules {
  override def applyRule(valueIn: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    valueIn <= value
  }
}

object AmtLTE extends RuleConfigReader {
  override val field = "amount.lte"

  override def fromConfig(config: Config): Option[AmtLTE] = {
    read[Long](config).map(AmtLTE(_))
  }
}

case class AmtWithFee(value: Boolean) extends WebhookEventRules {
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    value
  }
}

object AmtWithFee extends RuleConfigReader {
  override val field = "amount.withFee"

  override def fromConfig(config: Config): Option[AmtWithFee] = {
    read[Boolean](config).map(AmtWithFee(_))
  }
}

case class WithTxsOfTypes(value: Seq[Int]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]) = ???
}

object WithTxsOfTypes extends RuleConfigReader {
  override val field = "withTxsOfTypes"

  override def fromConfig(config: Config): Option[WithTxsOfTypes] = {
    read[Seq[Int]](config).map(WithTxsOfTypes(_))
  }
}

case class WithTxsOfAccs(value: Seq[String]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]) = ???
}

object WithTxsOfAccs extends RuleConfigReader {
  override val field = "withTxsOfAccs"

  override def fromConfig(config: Config): Option[WithTxsOfAccs] = {
    read[Seq[String]](config).map(WithTxsOfAccs(_))
  }
}

case class WithStateOfAccs(value: Seq[String]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, tx: ProcessedTransaction, accs: Set[Address]) = ???
}

object WithStateOfAccs extends RuleConfigReader {
  override val field = "withStateOfAccs"

  override def fromConfig(config: Config): Option[WithStateOfAccs] = {
    read[Seq[String]](config).map(WithStateOfAccs(_))
  }
}