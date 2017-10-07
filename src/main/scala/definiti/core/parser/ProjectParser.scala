package definiti.core.parser

import java.nio.file.{Files, Path}

import definiti.core._
import definiti.core.ast.pure._
import definiti.core.parser.antlr.{CoreDefinitionLexer, CoreDefinitionParser, DefinitiLexer, DefinitiParser}
import definiti.core.parser.api.CoreDefinitionASTParser
import definiti.core.parser.project.DefinitiASTParser
import definiti.core.utils.CollectionUtils._
import definiti.core.utils.ErrorListener
import org.antlr.v4.runtime._

private[core] case class ProjectParsingResult(
  root: PureRoot,
  core: Seq[PureClassDefinition]
)

private[core] class ProjectParser(configuration: Configuration) {
  private val source = configuration.source
  private val coreSource = configuration.apiSource
  private val definitiASTParser = new DefinitiASTParser(configuration)

  def buildAST(): Validated[ProjectParsingResult] = {
    Validated.both(buildDefinitiAST(), buildCoreDefinitionAST())
      .map { case (definitiAST, coreAST) => ProjectParsingResult(definitiAST, coreAST) }
  }

  private def buildDefinitiAST(): Validated[PureRoot] = {
    val sourceFiles = extractDefinitiFiles()
    val definitiAstEither = sourceFiles.map(path => parseDefinitiFile(path.toString))
    Validated
      .squash(definitiAstEither)
      .map(PureRoot)
  }

  private def extractDefinitiFiles(): Seq[Path] = {
    scalaSeq(Files.find(source, 1000, (path, _) => String.valueOf(path).endsWith(".def")))
  }

  private def parseDefinitiFile(source: String): Validated[PureRootFile] = {
    val errorListener = new ErrorListener(source)
    val parser = buildParser(source, new DefinitiLexer(_), new DefinitiParser(_), errorListener)
    val result: DefinitiParser.DefinitiContext = parser.definiti()
    if (errorListener.hasError) {
      Invalid(errorListener.errors.map(_.toError))
    } else {
      ValidValue(definitiASTParser.definitiContextToAST(result))
    }
  }

  private def buildCoreDefinitionAST(): Validated[Seq[PureClassDefinition]] = {
    val sourceFiles = extractCoreDefinitionFiles()
    val validatedCoreAst = sourceFiles.map(path => parseCoreDefinitionFile(path.toString))
    Validated.flatSquash(validatedCoreAst)
  }

  private def extractCoreDefinitionFiles(): Seq[Path] = {
    scalaSeq(Files.find(coreSource, 1000, (path, _) => String.valueOf(path).endsWith(".definition")))
  }

  private def parseCoreDefinitionFile(source: String): Validated[Seq[PureClassDefinition]] = {
    val errorListener = new ErrorListener(source)
    val parser = buildParser(source, new CoreDefinitionLexer(_), new CoreDefinitionParser(_), errorListener)
    val result: CoreDefinitionParser.CoreDefinitionContext = parser.coreDefinition()
    if (errorListener.hasError) {
      Invalid(errorListener.errors.map(_.toError))
    } else {
      ValidValue(CoreDefinitionASTParser.definitionContextToAST(result))
    }
  }

  private def buildParser[B <: Parser](source: String, newLexer: (CharStream) => Lexer, newParser: (TokenStream) => B, errorListener: ErrorListener): B = {
    val in = CharStreams.fromFileName(source)
    val lexer = newLexer(in)
    val tokens = new CommonTokenStream(lexer)
    val parser = newParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(errorListener)
    parser
  }
}
