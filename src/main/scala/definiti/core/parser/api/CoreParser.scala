package definiti.core.parser.api

import java.nio.file.{Files, Path}

import definiti.core._
import definiti.core.ast.pure.PureClassDefinition
import definiti.core.parser.ParserHelper
import definiti.core.parser.antlr.{CoreDefinitionLexer, CoreDefinitionParser}
import definiti.core.utils.CollectionUtils.scalaSeq
import definiti.core.utils.ErrorListener

private[core] class CoreParser(configuration: Configuration) {
  private val coreSource: Path = configuration.apiSource

  def parse(): Program[Seq[PureClassDefinition]] = Program.validated {
    val sourceFiles = extractCoreDefinitionFiles()
    val validatedCoreAst = sourceFiles.map(path => parseCoreDefinitionFile(path.toString))
    Validated.flatSquash(validatedCoreAst)
  }

  private def extractCoreDefinitionFiles(): Seq[Path] = {
    scalaSeq(Files.find(coreSource, 1000, (path, _) => String.valueOf(path).endsWith(".definition")))
  }

  private def parseCoreDefinitionFile(source: String): Validated[Seq[PureClassDefinition]] = {
    val errorListener = new ErrorListener(source)
    val parser = ParserHelper.buildParser(source, new CoreDefinitionLexer(_), new CoreDefinitionParser(_), errorListener)
    val result: CoreDefinitionParser.CoreDefinitionContext = parser.coreDefinition()
    if (errorListener.hasError) {
      Invalid(errorListener.errors.map(_.toError))
    } else {
      Valid(new CoreDefinitionASTParser(source).definitionContextToAST(result))
    }
  }
}
