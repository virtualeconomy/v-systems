package vsys.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import vsys.blockchain.transaction.ValidationError

trait WebhookEventRules {
    //def applyRule(valIn: Any*): Boolean
    //TO DO 
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

object WebhookEventRules {
  
  def apply(config: Config)(path: String): Either[ValidationError, WebhookEventRules] = path match {
    case "afterHeight" => Right(AfterHeight(config.as[Option[Long]](path).getOrElse(0)))
    case "afterTime" => Right(AfterTime(config.as[Option[Long]](path).getOrElse(0)))
    case "withTxs" => Right(WithTxs(config.as[Option[Boolean]](path).getOrElse(false)))
    case "withMintingTxs" => Right(WithMintingTxs(config.as[Option[Boolean]](path).getOrElse(false)))
    case "relatedAccount" => Right(RelatedAccs(config.as[Option[Seq[String]]](path).get))
    case "includeTypes" => Right(IncludeTypes(config.as[Option[Seq[Int]]](path).get))
    case "excludeTypes" => Right(ExcludeTypes(config.as[Option[Seq[Int]]](path).get))
    case "amount.gt" => Right(AmtGT(config.as[Option[Long]](path).getOrElse(0)))
    case "amount.lt" => Right(AmtLT(config.as[Option[Long]](path).getOrElse(6*10^9*10^8L)))
    case "amount.gte" => Right(AmtGTE(config.as[Option[Long]](path).getOrElse(0)))
    case "amount.lte" => Right(AmtLTE(config.as[Option[Long]](path).getOrElse(6*10^9*10^8L)))
    case "amount.withFee" => Right(AmtWithFee(config.as[Option[Boolean]](path).getOrElse(false)))
    case "withTxsOfTypes" => Right(WithTxsOfTypes(config.as[Option[Seq[Int]]](path).get))
    case "withTxsOfAccs" => Right(WithTxsOfAccs(config.as[Option[Seq[String]]](path).get))
    case "withStateOfAccs" => Right(WithStateOfAccs(config.as[Option[Seq[String]]](path).get))
    case _ => Left(ValidationError.InvalidRuleTypeError)
  }
}