package definiti.core.mock.antlr

import definiti.core.parser.antlr.DefinitiParser._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class NamedFunctionContextMock(
  nameToken: Token,
  genericTypeListContext: GenericTypeListContext,
  parameterListDefinitionContext: ParameterListDefinitionContext,
  genericTypeContext: GenericTypeContext,
  namedFunctionBodyContext: NamedFunctionBodyContext
) extends NamedFunctionContext(null, 0) {
  this.name = nameToken

  override def genericTypeList() = genericTypeListContext

  override def parameterListDefinition() = parameterListDefinitionContext

  override def genericType() = genericTypeContext

  override def namedFunctionBody() = namedFunctionBodyContext

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(nameToken)
}

case class NamedFunctionBodyContextMock(
  content: Either[ChainedExpressionContext, ExpressionContext]
) extends NamedFunctionBodyContext(null, 0) {
  override def chainedExpression() = content.left.toOption.orNull

  override def expression() = content.right.toOption.orNull
}

object NamedFunctionBodyContextMock {
  def apply(content: ChainedExpressionContext): NamedFunctionBodyContextMock = {
    new NamedFunctionBodyContextMock(Left(content))
  }

  def apply(content: ExpressionContext): NamedFunctionBodyContextMock = {
    new NamedFunctionBodyContextMock(Right(content))
  }
}