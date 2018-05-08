package definiti.core.parser.api

import definiti.common.ast.ClassDefinition
import definiti.common.program.Program
import definiti.common.validation.{Invalid, Valid, Validated}
import definiti.core._
import definiti.core.parser.ParserHelper
import definiti.core.parser.antlr.{CoreDefinitionLexer, CoreDefinitionParser}
import definiti.core.utils.ErrorListener

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
    val errorListener = new ErrorListener(path)
    val parser = ParserHelper.buildParser(Source.fromResource(path), new CoreDefinitionLexer(_), new CoreDefinitionParser(_), errorListener)
    val result: CoreDefinitionParser.CoreDefinitionContext = parser.coreDefinition()
    if (errorListener.hasError) {
      Invalid(errorListener.errors.map(_.toError))
    } else {
      Valid(new CoreDefinitionASTParser(path).definitionContextToAST(result))
    }
  }
}
