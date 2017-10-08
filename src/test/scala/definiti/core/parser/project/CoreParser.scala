package definiti.core.parser.project

import java.nio.file.{Files, Path, Paths}

import definiti.core.ReferenceContext
import definiti.core.ast.pure._
import definiti.core.parser.antlr.{CoreDefinitionLexer, CoreDefinitionParser}
import definiti.core.parser.api.CoreDefinitionASTParser
import definiti.core.utils.CollectionUtils.scalaSeq
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

trait CoreParser {
  lazy val core: Seq[PureClassDefinition] = {
    extractCoreDefinitionFiles()
      .map(_.toAbsolutePath.toString)
      .flatMap(parseCoreDefinitionFile)
  }

  lazy val coreContext: ReferenceContext = {
    ReferenceContext(
      classes = core,
      verifications = Seq.empty,
      namedFunctions = Seq.empty
    )
  }

  private def extractCoreDefinitionFiles(): Seq[Path] = {
    val source = Paths.get("src", "main", "resources", "api")
    scalaSeq(Files.find(source, 1000, (path, _) => String.valueOf(path).endsWith(".definition")))
  }

  private def parseCoreDefinitionFile(fileName: String): Seq[PureClassDefinition] = {
    val in = CharStreams.fromFileName(fileName)
    val lexer = new CoreDefinitionLexer(in)
    val tokens = new CommonTokenStream(lexer)
    val parser = new CoreDefinitionParser(tokens)
    val coreDefinition = parser.coreDefinition()
    new CoreDefinitionASTParser(fileName).definitionContextToAST(coreDefinition)
  }
}
