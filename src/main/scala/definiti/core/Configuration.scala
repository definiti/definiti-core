package definiti.core

import java.nio.file.{Files, Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.Logger
import definiti.common.control.ControlLevel
import definiti.common.plugin._
import definiti.common.program.ProgramConfiguration
import definiti.common.utils.CollectionUtils._
import definiti.core.plugin.serialization.JsonSerialization
import definiti.core.plugin.{GeneratorCommandPlugin, ParserCommandPlugin, ValidatorCommandPlugin}
import definiti.core.validation.Controls

import scala.util.{Failure, Success, Try}

trait Configuration {
  def source: Path

  def parsers: Seq[ParserPlugin]

  def validators: Seq[ValidatorPlugin]

  def generators: Seq[GeneratorPlugin]

  def contexts: Seq[ContextPlugin[_]]

  def programConfiguration: ProgramConfiguration
}

private[core] class FileConfiguration(externalConfig: Config) extends Configuration {
  private val logger = Logger(getClass)
  private val config: Config = externalConfig.getConfig("definiti.core")

  def this() {
    this(ConfigFactory.load())
  }

  val jsonSerialization = new JsonSerialization(this)

  val source: Path = getPathOrElse("source", Paths.get("src"))

  val parsers: Seq[ParserPlugin] = generateInstancesOf(classOf[ParserPlugin], "parsers")

  val validators: Seq[ValidatorPlugin] = generateInstancesOf(classOf[ValidatorPlugin], "validators")

  val generators: Seq[GeneratorPlugin] = generateInstancesOf(classOf[GeneratorPlugin], "generators")

  val contexts: Seq[ContextPlugin[_]] = generateInstancesOf(classOf[ContextPlugin[_]], "contexts")

  private val userFlags: Map[String, ControlLevel.Value] = extractMap("flags").flatMap { case (key, value) =>
    ControlLevel.fromString(value) match {
      case Some(level) =>
        Some(key -> level)
      case _ =>
        logger.warn(s"Unknown level ${value} for control ${key}, ignored")
        None
    }
  }

  val programConfiguration: ProgramConfiguration = ProgramConfiguration(
    controlLevel = getEnumeration("controlLevel", ControlLevel, ControlLevel.warning),
    fatalLevel = getEnumeration("fatalLevel", ControlLevel, ControlLevel.error),
    userFlags = userFlags,
    defaultLevels = {
      Controls.all
        .map { control => control.name -> control.defaultLevel }
        .toMap
    }
  )

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
    getDependencies(configurationKey).flatMap { dependency =>
      if (isCommandLine(dependency)) {
        generateInstanceForCommand(dependency, traitClass)
      } else {
        generateInstanceFromClass(dependency, traitClass)
      }
    }
  }

  private def isCommandLine(dependency: String): Boolean = {
    dependency.startsWith(">")
  }

  private def generateInstanceFromClass[A](className: String, traitClass: Class[A]): Option[A] = {
    Try(Class.forName(className)) match {
      case Success(clazz) =>
        if (traitClass.isAssignableFrom(clazz)) {
          Try(clazz.newInstance()) match {
            case Success(instance) =>
              Some(instance.asInstanceOf[A])
            case Failure(_) =>
              logger.warn(s"Class $className was found but no instance could be created. Do you have a constructor without parameter? The class will be ignored.")
              None
          }
        } else {
          logger.warn(s"Class $className was found but does not inherit from ${traitClass.getName}. The class will be ignored.")
          None
        }
      case Failure(_) =>
        logger.warn(s"Class $className was set as ${traitClass.getName} but was not found. The class will be ignored.")
        None
    }
  }

  private def generateInstanceForCommand[A](dependency: String, traitClass: Class[A]): Option[A] = {
    val command = dependency.substring(1).trim
    if (traitClass == classOf[ParserPlugin]) {
      Some(new ParserCommandPlugin(command, jsonSerialization).asInstanceOf[A])
    } else if (traitClass == classOf[ValidatorPlugin]) {
      Some(new ValidatorCommandPlugin(command, jsonSerialization).asInstanceOf[A])
    } else if (traitClass == classOf[GeneratorPlugin]) {
      Some(new GeneratorCommandPlugin(command, jsonSerialization).asInstanceOf[A])
    } else {
      logger.warn(s"Unexpected traitClass ${traitClass}")
      None
    }
  }

  private def getDependencies(configurationKey: String): Seq[String] = {
    if (config.hasPath(configurationKey)) {
      scalaSeq(config.getStringList(configurationKey))
    } else {
      Seq.empty
    }
  }

  private def extractMap(configurationKey: String): Map[String, String] = {
    val configurationMap = config.getConfig(configurationKey)
    scalaSeq(configurationMap.entrySet())
      .map(_.getKey)
      .map(key => key -> configurationMap.getString(key))
      .toMap
  }

  private def getEnumeration[A <: Enumeration](path: String, enumeration: A, defaultValue: A#Value): A#Value = {
    val stringValue = getStringOrElse(path, defaultValue.toString)
    enumeration.values.find(_.toString == stringValue) match {
      case Some(value) => value
      case None =>
        logger.warn(s"Unknown value ${stringValue} for ${path}, ignored")
        defaultValue
    }
  }
}
