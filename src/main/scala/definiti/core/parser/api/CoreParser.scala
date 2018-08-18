package definiti.core.parser.api

import definiti.common.ast.ClassDefinition
import definiti.common.program.Program
import definiti.common.validation.{Valid, Validated}
import definiti.core._

import scala.io.Source

private[core] class CoreParser(configuration: Configuration) {
  def parse(): Program[Seq[ClassDefinition]] = Program.validated {
    val sourceFiles = extractCoreDefinitionFiles()
    val validatedCoreAst = sourceFiles.map(parseCoreDefinitionFile)
    Validated.flatSquash(validatedCoreAst)
  }

  private def extractCoreDefinitionFiles(): Seq[String] = {
    Source.fromResource("api")
      .getLines()
      .toSeq
  }

  private def parseCoreDefinitionFile(path: String): Validated[Seq[ClassDefinition]] = {
    for {
      source <- Valid(Source.fromResource(path).mkString)
      tokens <- new LexerCore(path).parse(source)
      classDefinitions <- new ParserCore(path).parse(tokens)
    } yield {
      classDefinitions
    }
  }
}
