package definiti.core.generators.antlr

import definiti.core.mock.antlr.{NamedFunctionBodyContextMock, NamedFunctionContextMock}
import definiti.core.parser.antlr.DefinitiParser._
import org.scalacheck.Gen

object NamedFunctionContextGenerator {
  lazy val anyNamedFunctionContext: Gen[NamedFunctionContext] = for {
    name <- AntlrGenerator.anyIdentifierToken
    genericTypeListContext <- GenericTypesContextGenerators.anyGenericTypeListContext
    parameterListDefinitionContext <- FunctionContextGenerator.anyParameterListDefinitionContext
    genericTypeContext <- GenericTypesContextGenerators.anyGenericTypesContext
    namedFunctionBodyContext <- anyNamedFunctionBodyContext
  } yield {
    NamedFunctionContextMock(
      nameToken = name,
      genericTypeListContext = genericTypeListContext,
      parameterListDefinitionContext = parameterListDefinitionContext,
      genericTypeContext = genericTypeContext,
      namedFunctionBodyContext = namedFunctionBodyContext
    )
  }

  lazy val anyNamedFunctionBodyContext: Gen[NamedFunctionBodyContext] = for {
    chainedExpressionContext <- ExpressionContextGenerator.anyChainedExpressionContext
    oneExpression <- ExpressionContextGenerator.anyExpressionContext
    takeOneExpression <- Gen.oneOf(true, false)
  } yield {
    if (takeOneExpression) {
      NamedFunctionBodyContextMock(oneExpression)
    } else {
      NamedFunctionBodyContextMock(chainedExpressionContext)
    }
  }
}
