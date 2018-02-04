package definiti.core.generators.antlr

import definiti.core.generators.Generators
import definiti.core.mock.antlr._
import definiti.core.parser.antlr.DefinitiParser._
import org.scalacheck.Gen

object ExpressionContextGenerator {
  lazy val anyChainedExpressionContext: Gen[ChainedExpressionContext] = AntlrGenerator.genContext(for {
    numberOfExpressions <- Generators.decreasingFrequency(1, 10)
    expressionContexts <- Gen.listOfN(numberOfExpressions, anyExpressionContext)
  } yield {
    ChainedExpressionContextMock(expressionContexts)
  })

  lazy val anyExpressionContext: Gen[ExpressionContext] = anyExpressionContext(5)

  def anyExpressionContext(limit: Int): Gen[ExpressionContext] = {
    // Protection for recursion
    limit match {
      case 0 =>
        Gen.oneOf(
          anyStringExpressionContext,
          anyNumberExpressionContext,
          anyBooleanExpressionContext
          // For now, refuse variable expression because directly dependant of context
          //anyVariableExpressionContext
        )
      case n =>
        Gen.frequency(
          100 -> anyStringExpressionContext,
          100 -> anyNumberExpressionContext,
          100 -> anyBooleanExpressionContext,
          // For now, refuse variable expression because directly dependant of context
          //100 -> anyVariableExpressionContext,
          50 -> anyNotExpressionContext(n - 1),
          20 -> anyParenthesisExpressionContext(n - 1),
          12 -> anyAttributeCallExpressionContext(n - 1),
          10 -> anyBinaryOperatorExpressionContext(n - 1),
          10 -> anyMethodCallExpressionContext(n - 1),
          5 -> anyConditionExpressionContext(n - 1)
        )
    }
  }

  def anyLambdaExpressionContext(limit: Int): Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    parameterListDefinitionContext <- anyParameterListDefinitionContext(limit)
    lambdaExpressionContext <- anyExpressionContext(limit)
  } yield {
    LambdaExpressionContextMock(
      parameterListDefinitionContext,
      lambdaExpressionContext
    )
  })

  def anyParenthesisExpressionContext(limit: Int): Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    parenthesisContext <- anyExpressionContext(limit)
  } yield {
    ParenthesisExpressionContextMock(parenthesisContext)
  })

  def anyMethodCallExpressionContext(limit: Int): Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    methodExpressionContext <- anyExpressionContext(limit)
    methodNameToken <- AntlrGenerator.anyIdentifierToken
    genericTypeListContext <- GenericTypesContextGenerators.anyGenericTypeListContext
    methodExpressionParametersContext <- anyExpressionListContext(limit)
  } yield {
    MethodCallExpressionContextMock(
      methodExpressionContext,
      methodNameToken,
      genericTypeListContext,
      methodExpressionParametersContext
    )
  })

  def anyAttributeCallExpressionContext(limit: Int): Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    attributeExpressionContext <- anyExpressionContext(limit)
    attributeNameToken <- AntlrGenerator.anyIdentifierToken
  } yield {
    AttributeCallExpressionContextMock(
      attributeExpressionContext,
      attributeNameToken
    )
  })

  def anyNotExpressionContext(limit: Int): Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    notExpressionContext <- anyExpressionContext(limit)
  } yield {
    NotExpressionContextMock(notExpressionContext)
  })

  def anyBinaryOperatorExpressionContext(limit: Int): Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    leftExpressionContext <- anyExpressionContext(limit)
    operatorNode <- AntlrGenerator.anyBinaryOperatorToken
    rightExpressionContext <- anyExpressionContext(limit)
  } yield {
    BinaryOperatorExpressionContextMock(
      leftExpressionContext,
      operatorNode,
      rightExpressionContext
    )
  })

  lazy val anyBooleanExpressionContext: Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    boolean <- AntlrGenerator.anyBooleanNode
  } yield {
    BooleanExpressionContextMock(boolean)
  })

  lazy val anyNumberExpressionContext: Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    number <- AntlrGenerator.anyNumberNode
  } yield {
    NumberExpressionContextMock(number)
  })

  lazy val anyStringExpressionContext: Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    string <- AntlrGenerator.anyStringNode
  } yield {
    StringExpressionContextMock(string)
  })

  lazy val anyVariableExpressionContext: Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    variable <- AntlrGenerator.anyIdentifierToken
  } yield {
    ReferenceExpressionContextMock(variable)
  })

  def anyConditionExpressionContext(limit: Int): Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    conditionExpressionContext <- anyExpressionContext(limit)
    conditionIfBodyContext <- anyChainedExpressionContext
    conditionElseBodyContext <- Gen.option(anyChainedExpressionContext)
  } yield {
    ConditionExpressionContextMock(
      conditionExpressionContext,
      conditionIfBodyContext,
      conditionElseBodyContext
    )
  })

  lazy val anyFunctionCallContext: Gen[ExpressionContext] = anyFunctionCallContext(5)

  def anyFunctionCallContext(limit: Int): Gen[ExpressionContext] = AntlrGenerator.genContext(for {
    functionNameToken <- AntlrGenerator.anyIdentifierToken
    functionGenericsContext <- GenericTypesContextGenerators.anyGenericTypeListContext
    functionExpressionParametersContext <- anyExpressionListContext(limit)
  } yield {
    FunctionCallContextMock(
      functionNameToken = functionNameToken,
      functionGenericsContext = functionGenericsContext,
      functionExpressionParametersContext = functionExpressionParametersContext
    )
  })

  def anyParameterListDefinitionContext(limit: Int): Gen[ParameterListDefinitionContext] = AntlrGenerator.genContext(for {
    parameterDefinitionContexts <- Generators.listDecreasingFrequencySize(0, 5, anyParameterDefinitionContext(limit))
  } yield {
    ParameterListDefinitionContextMock(parameterDefinitionContexts)
  })

  def anyParameterDefinitionContext(limit: Int): Gen[ParameterDefinitionContext] = AntlrGenerator.genContext(for {
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

  def anyExpressionListContext(limit: Int): Gen[ExpressionListContext] = AntlrGenerator.genContext(for {
    expressionContexts <- Generators.listDecreasingFrequencySize(0, 5, anyExpressionContext(limit))
  } yield {
    ExpressionListContextMock(expressionContexts)
  })
}
