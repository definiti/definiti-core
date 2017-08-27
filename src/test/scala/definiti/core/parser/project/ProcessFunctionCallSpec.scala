package definiti.core.parser.project

import definiti.core._
import definiti.core.generators.antlr.ExpressionContextGenerator
import definiti.core.mock.antlr._
import definiti.core.parser.TestConstants.defaultRange
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ProcessFunctionCallSpec extends FlatSpec with Matchers with PropertyChecks {
  private val configuration = ConfigurationMock()
  private val definitiASTParser = new DefinitiASTParser(configuration)

  "DefinitiASTParser.processFunctionCall" should "return a FunctionCall without exception" in {
    forAll(ExpressionContextGenerator.anyFunctionCallContext) { functionCallContext =>
      definitiASTParser.processFunctionCall(functionCallContext)
    }
  }

  it should "map the ExpressionContext into FunctionCall" in {
    val input = FunctionCallContextMock(
      functionNameToken = TokenMock("myName"),
      functionGenericsContext = GenericTypeListContextMock(Seq(
        GenericTypeContextMock(
          identifier = TerminalNodeMock("A"),
          genericTypeListContext = None
        )
      )),
      functionExpressionParametersContext = ExpressionListContextMock(Seq(
        BooleanExpressionContextMock(TerminalNodeMock("true")),
        NumberExpressionContextMock(TerminalNodeMock("123"))
      ))
    )
    val expected = FunctionCall(
      name = "myName",
      parameters = Seq(
        BooleanValue(value = true, defaultRange),
        NumberValue(value = 123, defaultRange)
      ),
      generics = Seq(TypeReference("A", Seq.empty)),
      range = defaultRange
    )
    val output = definitiASTParser.processFunctionCall(input)
    output should equal(expected)
  }
}
