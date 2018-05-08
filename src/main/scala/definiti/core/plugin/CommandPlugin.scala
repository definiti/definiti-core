package definiti.core.plugin

import java.nio.file.Path

import definiti.common.ast.{Library, Root}
import definiti.common.plugin._
import definiti.common.program.ProgramResult.NoResult
import definiti.common.validation.{Valid, Validated}
import definiti.core.plugin.serialization.JsonSerialization

import scala.collection.mutable.ListBuffer
import scala.sys.process._

private[core] class ParserCommandPlugin(command: String, jsonSerialization: JsonSerialization) extends ParserPlugin {
  override def name: String = command

  private val commandPath: String = if (command.startsWith("./")) command else s"./${command}"

  override def transform(root: Root): Validated[Root] = {
    Command.execute(commandPath, "transform", jsonSerialization.rootToJson(root)) match {
      case CommandResult(0, out, _) => Valid(jsonSerialization.rootFromJson(out))
      case CommandResult(_, out, _) => jsonSerialization.invalidFromJson(out)
    }
  }
}

private[core] class ValidatorCommandPlugin(command: String, jsonSerialization: JsonSerialization) extends ValidatorPlugin {
  override def name: String = command

  private val commandPath: String = if (command.startsWith("./")) command else s"./${command}"

  override def validate(root: Root, library: Library): Validated[NoResult] = {
    Command.execute(commandPath, "validate", jsonSerialization.rootToJson(root), jsonSerialization.libraryToJson(library)) match {
      case CommandResult(0, _, _) => Valid(NoResult)
      case CommandResult(_, out, _) => jsonSerialization.invalidFromJson(out)
    }
  }
}

private[core] class GeneratorCommandPlugin(command: String, jsonSerialization: JsonSerialization) extends GeneratorPlugin {
  override def name: String = command

  private val commandPath: String = if (command.startsWith("./")) command else s"./${command}"

  override def generate(root: Root, library: Library): Map[Path, String] = {
    Command.execute(commandPath, "generate", jsonSerialization.rootToJson(root), jsonSerialization.libraryToJson(library)) match {
      case CommandResult(0, out, _) => jsonSerialization.filesFromJson(out)
      case _ => Map.empty
    }
  }
}

private[core] case class CommandResult(status: Int, out: String, err: String)

private[core] object Command {
  def execute(command: String, arguments: String*): CommandResult = {
    val logger = new CommandPluginProcessLogger
    val status = (command +: arguments).!(logger)
    CommandResult(status, logger.outString, logger.errString)
  }

  private class CommandPluginProcessLogger extends ProcessLogger {
    private val outBuffer = new ListBuffer[String]
    private val errBuffer = new ListBuffer[String]

    def out: Seq[String] = outBuffer.toList

    def outString: String = out.mkString("\n")

    def err: Seq[String] = errBuffer.toList

    def errString: String = err.mkString("\n")

    override def out(s: => String): Unit = outBuffer.append(s)

    override def err(s: => String): Unit = errBuffer.append(s)

    override def buffer[T](f: => T): T = f
  }

}
