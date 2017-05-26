package definiti.core.generators.antlr

import definiti.core.generators.Generators
import definiti.core.mock.antlr.{FunctionContextMock, ParameterDefinitionContextMock, ParameterListDefinitionContextMock}
import definiti.core.parser.antlr.DefinitiParser._
import org.scalacheck.Gen

object FunctionContextGenerator {
  lazy val anyFunctionContext: Gen[FunctionContext] = AntlrGenerator.genContext(for {
    parameterListDefinitionContext <- anyParameterListDefinitionContext
    chainedExpressionContext <- ExpressionContextGenerator.anyChainedExpressionContext
    genericTypeListContext <- GenericTypesContextGenerators.anyGenericTypeListContext
  } yield {
    FunctionContextMock(
      parameterListDefinitionContext,
      chainedExpressionContext,
      genericTypeListContext
    )
  })

  lazy val anyParameterListDefinitionContext: Gen[ParameterListDefinitionContext] = AntlrGenerator.genContext(for {
    parameterDefinitionContexts <- Generators.listOfBoundedSize(0, 3, anyParameterDefinitionContexts)
  } yield {
    ParameterListDefinitionContextMock(parameterDefinitionContexts)
  })

  lazy val anyParameterDefinitionContexts: Gen[ParameterDefinitionContext] = AntlrGenerator.genContext(for {
    parameterNameToken <- AntlrGenerator.anyIdentifierToken
    parameterTypeToken <- AntlrGenerator.anyIdentifierToken
    genericTypeListContexts <- GenericTypesContextGenerators.anyGenericTypeListContext
  } yield {
    ParameterDefinitionContextMock(
      parameterNameToken,
      parameterTypeToken,
      genericTypeListContexts
    )
  })
}