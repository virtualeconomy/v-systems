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
import vsys.utils.ScorexLogging

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
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
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
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    blockTime >= value
  }
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

case class Amount(value: Map[String, AnyVal]) extends WebhookEventRules {
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]): Boolean = {
    val (amt, fee) = TransactionHelper.extractAmtFee(tx)

    if (value.contains("withFee") && value("withFee").asInstanceOf[Boolean]) {
      // withFee
      value.toList.foldLeft(true) {
        case (accum, (k, v: Long)) => validateVal(k, v, amt + fee) && accum
        case (accum, _) => accum
      }
    } else {
      // not withFee
      value.foldLeft(true) {
        case (accum, (k, v: Long)) => validateVal(k , v, amt) && accum
        case (accum, _) => accum
      }
    }
  }

  private def validateVal(rule: String, target: Long, value: Long): Boolean = {
    rule match {
      case "gt" => value > target
      case "gte" => value >= target
      case "lt" => value < target
      case "lte" => value <= target
      case _ => false
    }
  }
}

object Amount extends RuleConfigReader {
  val compareRule = Seq("gt", "gte", "lt", "lte", "withFee")
  override val field = "amount"

  override def fromConfig(config: Config): Option[Amount] = {
    val amtRule = compareRule.foldLeft(Map[String, AnyVal]()) {case (accum, r) =>
      val newR = r match {
        case "gt" => getVal[Long](config, r).map(v => Map("gt" -> v)).getOrElse(Map[String, AnyVal]())
        case "gte" => getVal[Long](config, r).map(v => Map("gte" -> v)).getOrElse(Map[String, AnyVal]())
        case "lte" => getVal[Long](config, r).map(v => Map("lte" -> v)).getOrElse(Map[String, AnyVal]())
        case "lt" => getVal[Long](config, r).map(v => Map("lt" -> v)).getOrElse(Map[String, AnyVal]())
        case "withFee" => getVal[Boolean](config, r).map(v => Map("withFee" -> v)).getOrElse(Map[String, AnyVal]())
        case _ => Map[String, AnyVal]()
      }
      accum ++ newR
    }

    if (amtRule.nonEmpty) Some(Amount(amtRule)) else None
  }

  private def getVal[T: ValueReader](config: Config, r: String): Option[T] = {
    Try(config.as[T]("rules." + field + "." + r)).toOption
  }
}

case class WithTxsOfTypes(value: Seq[Int]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]) = ???
}

object WithTxsOfTypes extends RuleConfigReader {
  override val field = "withTxsOfTypes"

  override def fromConfig(config: Config): Option[WithTxsOfTypes] = {
    read[Seq[Int]](config).map(WithTxsOfTypes(_))
  }
}

case class WithTxsOfAccs(value: Seq[String]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]) = ???
}

object WithTxsOfAccs extends RuleConfigReader {
  override val field = "withTxsOfAccs"

  override def fromConfig(config: Config): Option[WithTxsOfAccs] = {
    read[Seq[String]](config).map(WithTxsOfAccs(_))
  }
}

case class WithStateOfAccs(value: Seq[String]) extends WebhookEventRules {
  // TO DO
  override def applyRule(height: Long, blockTime: Long, tx: ProcessedTransaction, accs: Set[Address]) = ???
}

object WithStateOfAccs extends RuleConfigReader {
  override val field = "withStateOfAccs"

  override def fromConfig(config: Config): Option[WithStateOfAccs] = {
    read[Seq[String]](config).map(WithStateOfAccs(_))
  }
}