package definiti.core.generators.antlr

import definiti.core.mock.antlr.NamedFunctionContextMock
import definiti.core.parser.antlr.DefinitiParser._
import org.scalacheck.Gen

object NamedFunctionContextGenerator {
  lazy val anyNamedFunctionContext: Gen[NamedFunctionContext] = for {
    name <- AntlrGenerator.anyIdentifierToken
    genericTypeListContext <- GenericTypesContextGenerators.anyGenericTypeListContext
    parameterListDefinitionContext <- FunctionContextGenerator.anyParameterListDefinitionContext
    genericTypeContext <- GenericTypesContextGenerators.anyGenericTypesContext
    chainedExpressionContext <- ExpressionContextGenerator.anyChainedExpressionContext
  } yield {
    NamedFunctionContextMock(
      nameToken = name,
      genericTypeListContext = genericTypeListContext,
      parameterListDefinitionContext = parameterListDefinitionContext,
      genericTypeContext = genericTypeContext,
      chainedExpressionContext = chainedExpressionContext
    )
  }
}
