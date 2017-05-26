package definiti.core.mock.antlr

import java.util.{List => JList}

import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class FunctionContextMock(
  parameterListDefinitionContext: ParameterListDefinitionContext,
  chainedExpressionContext: ChainedExpressionContext,
  genericTypeListContext: GenericTypeListContext
) extends FunctionContext(null, 0) {
  override def parameterListDefinition(): ParameterListDefinitionContext = parameterListDefinitionContext

  override def chainedExpression(): ChainedExpressionContext = chainedExpressionContext

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext
}

case class ParameterListDefinitionContextMock(
  parameterDefinitionContexts: Seq[ParameterDefinitionContext]
) extends ParameterListDefinitionContext(null, 0) {
  override def parameterDefinition(): JList[ParameterDefinitionContext] = javaList(parameterDefinitionContexts)

  override def parameterDefinition(i: Int): ParameterDefinitionContext = parameterDefinitionContexts(i)
}

case class ParameterDefinitionContextMock(
  parameterNameToken: Token,
  parameterTypeToken: Token,
  genericTypeListContext: GenericTypeListContext
) extends ParameterDefinitionContext(null, 0) {
  this.parameterName = parameterNameToken
  this.parameterType = parameterTypeToken

  override def IDENTIFIER(): JList[TerminalNode] = javaList(Seq(TerminalNodeMock(parameterNameToken), TerminalNodeMock(parameterTypeToken)))

  override def IDENTIFIER(i: Int): TerminalNode = IDENTIFIER().get(i)

  override def genericTypeList(): GenericTypeListContext = genericTypeListContext
}