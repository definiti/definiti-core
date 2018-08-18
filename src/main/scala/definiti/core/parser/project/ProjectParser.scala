package definiti.core.parser.project

import java.nio.file.{Files, Path}

import definiti.common.ast.{Namespace, Root}
import definiti.common.program.Program
import definiti.common.utils.CollectionUtils._
import definiti.common.validation.{Valid, Validated}
import definiti.core.Configuration

import scala.io.{Codec, Source}

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

  private def parseDefinitiFile(path: String): Validated[Namespace] = {
    for {
      source <- Valid(Source.fromFile(path, Codec.UTF8.toString()).mkString)
      tokens <- new LexerProject(path).parse(source)
      tokenProjectReader = new TokenProjectReader(tokens)
      fileContent <- new DefinitiFileParser(path).parse(tokenProjectReader)
      namespace = new NamespaceBuilder(fileContent, configuration).build()
    } yield {
      namespace
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
