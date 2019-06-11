package scorex.api.http.swagger

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.github.swagger.akka.model.{Info, License}
import com.github.swagger.akka.SwaggerHttpService
import com.wavesplatform.Version
import com.wavesplatform.settings.RestAPISettings
import io.swagger.util.{Json, Yaml}
import io.swagger.models.{Swagger, Scheme, Path}
import scorex.utils.ScorexLogging
import scala.collection.immutable.Map

import scala.util.control.NonFatal
import scala.collection.JavaConverters._

class SwaggerDocService(val actorSystem: ActorSystem, val materializer: ActorMaterializer, val apiClasses: Set[Class[_]], settings: RestAPISettings)
  extends SwaggerHttpService with ScorexLogging {

  override val host: String = settings.bindAddress + ":" + settings.port
  override val info: Info = Info("The Web Interface to the VSYS Full Node API",
    Version.VersionString,
    "VSYS Full Node",
    "License: MIT License",
    None,
    Some(License("MIT License", "https://github.com/virtualeconomy/vsys/blob/master/LICENSE"))
  )

  override def generateSwaggerJson: String = {
    try {
      Json.pretty().writeValueAsString(customizedSwagger)
    } catch {
      case NonFatal(t) => {
        log.error("Issue with creating swagger.json", t)
        throw t
      }
    }
  }

  override def generateSwaggerYaml: String = {
    try {
      Yaml.pretty().writeValueAsString(customizedSwagger)
    } catch {
      case NonFatal(t) => {
        log.error("Issue with creating swagger.yaml", t)
        throw t
      }
    }
  }

  private val cumstomPathsConfig: Map[String, Boolean] = Map(
    "/transactions/count"  -> settings.customApiSettings.transactionsApiSettings.addressTransactionCount,
    "/transactions/list"   -> settings.customApiSettings.transactionsApiSettings.addressTransactionList,
  )

  private def customizedSwagger: Swagger = {
    val swagger: Swagger = reader.read(apiClasses.asJava)
    val fiteredPaths: Map[String, Path] = swagger.getPaths().asScala.toMap.filterKeys(cumstomPathsConfig.getOrElse(_, true))
    swagger.paths(fiteredPaths.asJava)
    swagger
  }

  //Let swagger-ui determine the host and port
  override val swaggerConfig: Swagger = new Swagger()
    .basePath(SwaggerHttpService.prependSlashIfNecessary(basePath))
    .info(info)
    .scheme(Scheme.HTTP)
    .scheme(Scheme.HTTPS)
}
