package definiti.core.parser.project

import java.nio.file.{Files, Path}

import definiti.common.ast.{Namespace, Root}
import definiti.common.program.Program
import definiti.common.utils.CollectionUtils._
import definiti.common.validation.{Invalid, Valid, Validated}
import definiti.core.Configuration
import definiti.core.parser.ParserHelper
import definiti.core.parser.antlr.{DefinitiLexer, DefinitiParser}
import definiti.core.utils.ErrorListener

private[core] class ProjectParser(configuration: Configuration) {
  private val source: Path = configuration.source

  def parse(): Program[Root] = Program.validated {
    val sourceFiles = extractDefinitiFiles()
    val validatedNamespace = sourceFiles.map(path => parseDefinitiFile(path.toString))
    Validated
      .squash(validatedNamespace)
      .map(mergeNamespaces)
      .map(Root)
  }

  private def extractDefinitiFiles(): Seq[Path] = {
    scalaSeq(Files.find(source, 1000, (path, _) => String.valueOf(path).endsWith(".def")))
  }

  private def parseDefinitiFile(source: String): Validated[Namespace] = {
    val errorListener = new ErrorListener(source)
    val parser = ParserHelper.buildParser(source, new DefinitiLexer(_), new DefinitiParser(_), errorListener)
    val result: DefinitiParser.DefinitiContext = parser.definiti()
    if (errorListener.hasError) {
      Invalid(errorListener.errors.map(_.toError))
    } else {
      Valid(new DefinitiASTParser(source, configuration).definitiContextToAST(result))
    }
  }

  private def mergeNamespaces(namespaces: Seq[Namespace]): Seq[Namespace] = {
    namespaces.foldLeft(Map.empty[String, Namespace]) { (acc, current) =>
      acc.get(current.fullName) match {
        case Some(existingNamespace) =>
          acc + (existingNamespace.fullName -> existingNamespace.copy(
            elements = existingNamespace.elements ++ current.elements
          ))
        case None =>
          acc + (current.fullName -> current)
      }
    }.values.toSeq
  }
}
