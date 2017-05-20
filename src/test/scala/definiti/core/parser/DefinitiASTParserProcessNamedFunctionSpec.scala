package definiti.core.parser

import definiti.core.generators.antlr.NamedFunctionContextGenerator
import definiti.core.mock.antlr._
import definiti.core.parser.TestConstants._
import definiti.core.{BooleanValue, DefinedFunction, NamedFunction}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class DefinitiASTParserProcessNamedFunctionSpec extends FlatSpec with Matchers with PropertyChecks {
  "DefinitiASTParser.processNamedFunction" should "return a NamedFunction without exception" in {
    forAll(NamedFunctionContextGenerator.anyNamedFunctionContext) { namedFunctionContext =>
      DefinitiASTParser.processNamedFunction(namedFunctionContext)
    }
  }

  it should "map the NamedFunctionContext into NamedFunction" in {
    val input = NamedFunctionContextMock(
      nameToken = TokenMock("myName"),
      functionContext = FunctionContextMock(
        parameterListDefinitionContext = ParameterListDefinitionContextMock(Seq.empty),
        chainedExpressionContext = ChainedExpressionContextMock(Seq(
          BooleanExpressionContextMock(TerminalNodeMock(TokenMock("true")))
        )),
        genericTypeListContext = GenericTypeListContextMock(Seq.empty)
      )
    )
    val expected = NamedFunction(
      name = "myName",
      function = DefinedFunction(
        parameters = Seq.empty,
        body = BooleanValue(value = true, defaultRange),
        genericTypes = Seq.empty,
        range = defaultRange
      ),
      range = defaultRange
    )
    val output = DefinitiASTParser.processNamedFunction(input)
    output should equal(expected)
  }
}
