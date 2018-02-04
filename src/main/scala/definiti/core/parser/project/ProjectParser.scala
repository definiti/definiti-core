package definiti.core.parser.project

import java.nio.file.{Files, Path}

import definiti.core._
import definiti.core.ast.pure._
import definiti.core.parser.ParserHelper
import definiti.core.parser.antlr.{DefinitiLexer, DefinitiParser}
import definiti.core.utils.CollectionUtils._
import definiti.core.utils.ErrorListener

private[core] class ProjectParser(configuration: Configuration) {
  private val source: Path = configuration.source

  def parse(): Program[PureRoot] = Program.validated {
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
    val parser = ParserHelper.buildParser(source, new DefinitiLexer(_), new DefinitiParser(_), errorListener)
    val result: DefinitiParser.DefinitiContext = parser.definiti()
    if (errorListener.hasError) {
      Invalid(errorListener.errors.map(_.toError))
    } else {
      Valid(new DefinitiASTParser(source, configuration).definitiContextToAST(result))
    }
  }
}
