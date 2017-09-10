package definiti.core.mock.antlr

import definiti.core.parser.antlr.DefinitiParser._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class NamedFunctionContextMock(
  nameToken: Token,
  genericTypeListContext: GenericTypeListContext,
  parameterListDefinitionContext: ParameterListDefinitionContext,
  genericTypeContext: GenericTypeContext,
  chainedExpressionContext: ChainedExpressionContext
) extends NamedFunctionContext(null, 0) {
  this.name = nameToken

  override def genericTypeList() = genericTypeListContext

  override def parameterListDefinition() = parameterListDefinitionContext

  override def genericType() = genericTypeContext

  override def chainedExpression() = chainedExpressionContext

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(nameToken)
}
