package definiti.core.parser

import java.nio.file.{Files, Path}

import definiti.core.parser.antlr.{CoreDefinitionLexer, CoreDefinitionParser, DefinitiLexer, DefinitiParser}
import definiti.core.parser.api.CoreDefinitionASTParser
import definiti.core.parser.project.DefinitiASTParser
import definiti.core.utils.{ErrorItem, ErrorListener}
import definiti.core.{ClassDefinition, Configuration, Root, RootFile}
import org.antlr.v4.runtime._
import definiti.core.utils.CollectionUtils._

private[core] case class ProjectParsingResult(
  root: Root,
  core: Seq[ClassDefinition]
)

private[core] class ProjectParser(configuration: Configuration) {
  def buildAST(): Either[Seq[ErrorItem], ProjectParsingResult] = {
    (buildDefinitiAST(), buildCoreDefinitionAST()) match {
      case (Left(definitiErrors), Left(coreErrors)) => Left(definitiErrors ++ coreErrors)
      case (Left(definitiErrors), _) => Left(definitiErrors)
      case (_, Left(coreErrors)) => Left(coreErrors)
      case (Right(definitiAST), Right(coreAST)) => Right(ProjectParsingResult(definitiAST, coreAST))
    }
  }

  private def buildDefinitiAST(): Either[Seq[ErrorItem], Root] = {
    val sourceFiles = extractDefinitiFiles()
    val definitiAstEither = sourceFiles.map(path => parseDefinitiFile(path.toString))
    if (definitiAstEither.forall(_.isRight)) {
      val allAST = definitiAstEither.map(_.right.get)
      Right(Root(allAST))
    } else {
      val allErrors = definitiAstEither.collect {
        case Left(errors) => errors
      }.flatten
      Left(allErrors)
    }
  }

  private def extractDefinitiFiles(): Seq[Path] = {
    scalaSeq(Files.find(configuration.source, 1000, (path, _) => String.valueOf(path).endsWith(".def")))
  }

  private def parseDefinitiFile(source: String): Either[Seq[ErrorItem], RootFile] = {
    val errorListener = new ErrorListener(source)
    val parser = buildParser(source, new DefinitiLexer(_), new DefinitiParser(_), errorListener)
    val result: DefinitiParser.DefinitiContext = parser.definiti()
    if (errorListener.hasError) {
      Left(errorListener.errors)
    } else {
      Right(DefinitiASTParser.definitiContextToAST(result))
    }
  }

  private def buildCoreDefinitionAST(): Either[Seq[ErrorItem], Seq[ClassDefinition]] = {
    val sourceFiles = extractCoreDefinitionFiles()
    val coreAstEither = sourceFiles.map(path => parseCoreDefinitionFile(path.toString))
    if (coreAstEither.forall(_.isRight)) {
      val allCoreAST = coreAstEither.flatMap(_.right.get)
      Right(allCoreAST)
    } else {
      val allErrors = coreAstEither.collect {
        case Left(errors) => errors
      }.flatten
      Left(allErrors)
    }
  }

  private def extractCoreDefinitionFiles(): Seq[Path] = {
    scalaSeq(Files.find(configuration.core.source, 1000, (path, _) => String.valueOf(path).endsWith(".definition")))
  }

  private def parseCoreDefinitionFile(source: String): Either[Seq[ErrorItem], Seq[ClassDefinition]] = {
    val errorListener = new ErrorListener(source)
    val parser = buildParser(source, new CoreDefinitionLexer(_), new CoreDefinitionParser(_), errorListener)
    val result: CoreDefinitionParser.CoreDefinitionContext = parser.coreDefinition()
    if (errorListener.hasError) {
      Left(errorListener.errors)
    } else {
      Right(CoreDefinitionASTParser.definitionContextToAST(result))
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
