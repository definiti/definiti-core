package definiti.core.generators.antlr

import definiti.core.mock.antlr.NamedFunctionContextMock
import definiti.core.parser.antlr.DefinitiParser.NamedFunctionContext
import org.scalacheck.Gen

object NamedFunctionContextGenerator {
  lazy val anyNamedFunctionContext: Gen[NamedFunctionContext] = for {
    name <- AntlrGenerator.anyIdentifierToken
    function <- FunctionContextGenerator.anyFunctionContext
  } yield {
    NamedFunctionContextMock(
      nameToken = name,
      functionContext = function
    )
  }
}
