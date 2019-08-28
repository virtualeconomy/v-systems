package vsys.settings

import com.typesafe.config.Config
import scala.util.Try
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader

trait WebhookEventRules extends Product with Serializable {
  //override def applyRule(valIn: Any*): Boolean
  //TO DO
  val value: Any 
}

trait EventConfigReader {
  val field: String
  private val path = "rules"
  lazy val valPath = path + "." + field

  def fromConfig(config: Config): Option[WebhookEventRules]

  def read[T: ValueReader](config: Config): Option[T] = {
    Try(config.as[T](valPath)).toOption
  }
}


case class AfterHeight(value: Long) extends WebhookEventRules
case class AfterTime(value: Long) extends WebhookEventRules
case class WithTxs(value: Boolean) extends WebhookEventRules
case class WithMintingTxs(value: Boolean) extends WebhookEventRules
case class RelatedAccs(value: Seq[String]) extends WebhookEventRules
case class IncludeTypes(value: Seq[Int]) extends WebhookEventRules
case class ExcludeTypes(value: Seq[Int]) extends WebhookEventRules
case class AmtGT(value: Long) extends WebhookEventRules
case class AmtLT(value: Long) extends WebhookEventRules
case class AmtGTE(value: Long) extends WebhookEventRules
case class AmtLTE(value: Long) extends WebhookEventRules
case class AmtWithFee(value: Boolean) extends WebhookEventRules
case class WithTxsOfTypes(value: Seq[Int]) extends WebhookEventRules
case class WithTxsOfAccs(value: Seq[String]) extends WebhookEventRules
case class WithStateOfAccs(value: Seq[String]) extends WebhookEventRules



object AfterHeight extends EventConfigReader {
  override val field = "afterHeight"

  override def fromConfig(config: Config): Option[AfterHeight] = {
    read[Long](config).map(AfterHeight(_))
  }
}

object AfterTime extends EventConfigReader {
  override val field = "afterTime"

  override def fromConfig(config: Config): Option[AfterTime] = {
    read[Long](config).map(AfterTime(_))
  }
}

object WithTxs extends EventConfigReader {
  override val field = "withTxs"

  override def fromConfig(config: Config): Option[WithTxs] = {
    read[Boolean](config).map(WithTxs(_))
  }
}

object WithMintingTxs extends EventConfigReader {
  override val field = "withMintingTxs"

  override def fromConfig(config: Config): Option[WithMintingTxs] = {
    read[Boolean](config).map(WithMintingTxs(_))
  }
}

object RelatedAccs extends EventConfigReader {
  override val field = "relatedAccount"

  override def fromConfig(config: Config): Option[RelatedAccs] = {
    read[Seq[String]](config).map(RelatedAccs(_))
  }
}

object IncludeTypes extends EventConfigReader {
  override val field = "includeTypes"

  override def fromConfig(config: Config): Option[IncludeTypes] = {
    read[Seq[Int]](config).map(IncludeTypes(_))
  }
}

object ExcludeTypes extends EventConfigReader {
  override val field = "excludeTypes"

  override def fromConfig(config: Config): Option[ExcludeTypes] = {
    read[Seq[Int]](config).map(ExcludeTypes(_))
  }
}

object AmtGT extends EventConfigReader {
  override val field = "amount.gt"

  override def fromConfig(config: Config): Option[AmtGT] = {
    read[Long](config).map(AmtGT(_))
  }
}

object AmtLT extends EventConfigReader {
  override val field = "amount.lt"

  override def fromConfig(config: Config): Option[AmtLT] = {
    read[Long](config).map(AmtLT(_))
  }
}

object AmtGTE extends EventConfigReader {
  override val field = "amount.gte"

  override def fromConfig(config: Config): Option[AmtGTE] = {
    read[Long](config).map(AmtGTE(_))
  }
}

object AmtLTE extends EventConfigReader {
  override val field = "amount.lte"

  override def fromConfig(config: Config): Option[AmtLTE] = {
    read[Long](config).map(AmtLTE(_))
  }
}

object AmtWithFee extends EventConfigReader {
  override val field = "amount.withFee"

  override def fromConfig(config: Config): Option[AmtWithFee] = {
    read[Boolean](config).map(AmtWithFee(_))
  }
}

object WithTxsOfTypes extends EventConfigReader {
  override val field = "withTxsOfTypes"

  override def fromConfig(config: Config): Option[WithTxsOfTypes] = {
    read[Seq[Int]](config).map(WithTxsOfTypes(_))
  }
}

object WithTxsOfAccs extends EventConfigReader {
  override val field = "withTxsOfAccs"

  override def fromConfig(config: Config): Option[WithTxsOfAccs] = {
    read[Seq[String]](config).map(WithTxsOfAccs(_))
  }
}

object WithStateOfAccs extends EventConfigReader {
  override val field = "withStateOfAccs"

  override def fromConfig(config: Config): Option[WithStateOfAccs] = {
    read[Seq[String]](config).map(WithStateOfAccs(_))
  }
}