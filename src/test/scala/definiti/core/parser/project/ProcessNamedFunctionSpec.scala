package definiti.core.parser.project

import definiti.core.ConfigurationMock
import definiti.core._
import definiti.core.ast._
import definiti.core.ast.pure._
import definiti.core.generators.antlr.NamedFunctionContextGenerator
import definiti.core.mock.antlr._
import definiti.core.parser.TestConstants._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ProcessNamedFunctionSpec extends FlatSpec with Matchers with PropertyChecks {
  private val configuration = ConfigurationMock()
  private val definitiASTParser = new DefinitiASTParser(configuration)

  "DefinitiASTParser.processNamedFunction" should "return a NamedFunction without exception" in {
    forAll(NamedFunctionContextGenerator.anyNamedFunctionContext) { namedFunctionContext =>
      definitiASTParser.processNamedFunction(namedFunctionContext)
    }
  }

  it should "map the NamedFunctionContext into NamedFunction" in {
    val input = NamedFunctionContextMock(
      nameToken = TokenMock("myName"),
      parameterListDefinitionContext = ParameterListDefinitionContextMock(Seq.empty),
      namedFunctionBodyContext = NamedFunctionBodyContextMock(ChainedExpressionContextMock(Seq(
        BooleanExpressionContextMock(TerminalNodeMock(TokenMock("true")))
      ))),
      genericTypeContext = GenericTypeContextMock(
        identifier = TerminalNodeMock(TokenMock("Boolean")),
        genericTypeListContext = None
      ),
      genericTypeListContext = GenericTypeListContextMock(Seq.empty)
    )
    val expected = PureNamedFunction(
      name = "myName",
      packageName = NOT_DEFINED,
      parameters = Seq.empty,
      body = PureBooleanValue(value = true, defaultRange),
      genericTypes = Seq.empty,
      returnType = TypeReference("Boolean", Seq.empty),
      range = defaultRange
    )
    val output = definitiASTParser.processNamedFunction(input)
    output should equal(expected)
  }
}
