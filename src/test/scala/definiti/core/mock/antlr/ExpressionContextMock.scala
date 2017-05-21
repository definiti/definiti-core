package definiti.core.mock.antlr

import java.util.{List => JList}

import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class ChainedExpressionContextMock(
  expressionContexts: Seq[ExpressionContext]
) extends ChainedExpressionContext(null, 0) {
  override def expression(): JList[ExpressionContext] = javaList(expressionContexts)

  override def expression(i: Int): ExpressionContext = expressionContexts(i)
}

abstract class ExpressionContextMock extends ExpressionContext(null, 0) {
  this.methodExpression = null
  this.attributeExpression = null
  this.leftExpression = null
  this.lambdaExpression = null
  this.parenthesis = null
  this.notExpression = null
  this.booleanExpression = null
  this.numberExpression = null
  this.stringExpression = null
  this.referenceExpression = null
  this.conditionExpression = null
  this.conditionIfBody = null
  this.conditionElseBody = null
  this.operator = null
  this.rightExpression = null
  this.methodName = null
  this.methodExpressionParameters = null
  this.attributeName = null
  this.functionName = null
  this.functionGenerics = null
  this.functionExpressionParameters = null

  override def parameterListDefinition(): ParameterListDefinitionContext = null

  override def expression(): JList[ExpressionContext] = javaList(Seq())

  override def expression(i: Int): ExpressionContext = null

  override def BOOLEAN(): TerminalNode = null

  override def NUMBER(): TerminalNode = null

  override def STRING(): TerminalNode = null

  override def IDENTIFIER(): TerminalNode = null

  override def chainedExpression(): JList[ChainedExpressionContext] = javaList(Seq())

  override def chainedExpression(i: Int): ChainedExpressionContext = null

  override def CALCULATOR_OPERATOR_LEVEL_1(): TerminalNode = null

  override def CALCULATOR_OPERATOR_LEVEL_2(): TerminalNode = null

  override def LOGICAL_OPERATOR(): TerminalNode = null

  override def LOGICAL_COMBINATION_OPERATOR(): TerminalNode = null

  override def genericTypeList(): GenericTypeListContext = null

  override def expressionList(): ExpressionListContext = null
}

case class LambdaExpressionContextMock(
  parameterListDefinitionContext: ParameterListDefinitionContext,
  lambdaExpressionContext: ExpressionContext
) extends ExpressionContextMock {
  this.lambdaExpression = lambdaExpressionContext

  override def parameterListDefinition(): ParameterListDefinitionContext = parameterListDefinitionContext
}

case class ParenthesisExpressionContextMock(
  parenthesisContext: ExpressionContext
) extends ExpressionContextMock {
  this.parenthesis = parenthesisContext
}

case class MethodCallExpressionContextMock(
  methodExpressionContext: ExpressionContext,
  methodNameToken: Token,
  genericTypeListContext: GenericTypeListContext,
  methodExpressionParametersContext: ExpressionListContext
) extends ExpressionContextMock {
  this.methodExpression = methodExpressionContext
  this.methodName = methodNameToken
  this.methodExpressionParameters = methodExpressionParametersContext

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext
}

case class AttributeCallExpressionContextMock(
  attributeExpressionContext: ExpressionContext,
  attributeNameToken: Token
) extends ExpressionContextMock {
  this.attributeExpression = attributeExpressionContext
  this.attributeName = attributeNameToken
}

case class NotExpressionContextMock(
  notExpressionContext: ExpressionContext
) extends ExpressionContextMock {
  this.notExpression = notExpressionContext
}

case class BinaryOperatorExpressionContextMock(
  leftExpressionContext: ExpressionContext,
  operatorNode: Token,
  rightExpressionContext: ExpressionContext
) extends ExpressionContextMock {
  this.leftExpression = leftExpressionContext
  this.operator = operatorNode
  this.rightExpression = rightExpressionContext
}

case class BooleanExpressionContextMock(
  boolean: TerminalNode
) extends ExpressionContextMock {
  this.booleanExpression = boolean.getSymbol

  override def BOOLEAN(): TerminalNode = boolean
}

case class NumberExpressionContextMock(
  number: TerminalNode
) extends ExpressionContextMock {
  this.numberExpression = number.getSymbol

  override def NUMBER(): TerminalNode = number
}

case class StringExpressionContextMock(
  string: TerminalNode
) extends ExpressionContextMock {
  this.stringExpression = string.getSymbol

  override def STRING(): TerminalNode = string
}

case class ReferenceExpressionContextMock(
  reference: Token
) extends ExpressionContextMock {
  this.referenceExpression = reference
}

case class ConditionExpressionContextMock(
  conditionExpressionContext: ExpressionContext,
  conditionIfBodyContext: ChainedExpressionContext,
  conditionElseBodyContext: Option[ChainedExpressionContext]
) extends ExpressionContextMock {
  this.conditionExpression = conditionExpressionContext
  this.conditionIfBody = conditionIfBodyContext
  this.conditionElseBody = conditionElseBodyContext.orNull
}

case class ExpressionListContextMock(
  expressionContexts: Seq[ExpressionContext]
) extends ExpressionListContext(null, 0) {
  override def expression(): JList[ExpressionContext] = javaList(expressionContexts)

  override def expression(i: Int): ExpressionContext = expressionContexts(i)
}

case class FunctionCallContextMock(
  functionNameToken: Token,
  functionGenericsContext: GenericTypeListContext,
  functionExpressionParametersContext: ExpressionListContext
) extends ExpressionContextMock {
  this.functionName = functionNameToken
  this.functionGenerics = functionGenericsContext
  this.functionExpressionParameters = functionExpressionParametersContext
}