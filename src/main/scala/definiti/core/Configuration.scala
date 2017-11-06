package definiti.core

import java.nio.file.{Files, Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.Logger
import definiti.core.utils.CollectionUtils._

import scala.util.{Failure, Success, Try}

private[core] trait Configuration {
  def source: Path

  def apiSource: Path

  def parsers: Seq[ParserPlugin]

  def validators: Seq[ValidatorPlugin]

  def generators: Seq[GeneratorPlugin]

  def contexts: Seq[ContextPlugin[_]]
}

private[core] class FileConfiguration(externalConfig: Config) extends Configuration{
  private val logger = Logger(getClass)
  private val config: Config = externalConfig.getConfig("definiti.core")

  def this() {
    this(ConfigFactory.load())
  }

  lazy val source: Path = getPathOrElse("source", Paths.get("src"))

  lazy val apiSource: Path = getPathOrElse("api", Paths.get("src/main/resources/api"))

  lazy val parsers: Seq[ParserPlugin] = generateInstancesOf(classOf[ParserPlugin], "parsers")

  lazy val validators: Seq[ValidatorPlugin] = generateInstancesOf(classOf[ValidatorPlugin], "validators")

  lazy val generators: Seq[GeneratorPlugin] = generateInstancesOf(classOf[GeneratorPlugin], "generators")

  lazy val contexts: Seq[ContextPlugin[_]] = generateInstancesOf(classOf[ContextPlugin[_]], "contexts")

  private def getPathOrElse(configurationPath: String, defaultValue: => Path): Path = {
    if (config.hasPath(configurationPath)) {
      val rawPath = config.getString(configurationPath)
      val path = Paths.get(rawPath)
      if (Files.exists(path)) {
        path
      } else {
        logger.warn(s"Path $rawPath not found, using default one.")
        defaultValue
      }
    } else {
      defaultValue
    }
  }

  private def getStringOrElse(configurationPath: String, defaultValue: => String): String = {
    if (config.hasPath(configurationPath)) {
      config.getString(configurationPath)
    } else {
      defaultValue
    }
  }

  private def generateInstancesOf[A](traitClass: Class[A], configurationKey: String): Seq[A] = {
    getClassNamesFromConfiguration(configurationKey).flatMap { generatorClass =>
      Try(Class.forName(generatorClass)) match {
        case Success(clazz) =>
          if (traitClass.isAssignableFrom(clazz)) {
            Try(clazz.newInstance()) match {
              case Success(instance) =>
                Some(instance.asInstanceOf[A])
              case Failure(_) =>
                logger.warn(s"Class $generatorClass was found but no instance could be created. Do you have a constructor without parameter? The class will be ignored.")
                None
            }
          } else {
            logger.warn(s"Class $generatorClass was found but does not inherit from ${traitClass.getName}. The class will be ignored.")
            None
          }
        case Failure(_) =>
          logger.warn(s"Class $generatorClass was set as ${traitClass.getName} but was not found. The class will be ignored.")
          None
      }
    }
  }

  private def getClassNamesFromConfiguration(configurationKey: String): Seq[String] = {
    if (config.hasPath(configurationKey)) {
      scalaSeq(config.getStringList(configurationKey))
    } else {
      Seq.empty
    }
  }
}
