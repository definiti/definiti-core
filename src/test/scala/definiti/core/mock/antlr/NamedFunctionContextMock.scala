package definiti.core.mock.antlr

import definiti.core.parser.antlr.DefinitiParser
import definiti.core.parser.antlr.DefinitiParser.{FunctionContext, NamedFunctionContext}
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class NamedFunctionContextMock(
  nameToken: Token,
  functionContext: FunctionContext
) extends NamedFunctionContext(null, 0) {
  this.name = nameToken

  override def function(): DefinitiParser.FunctionContext = functionContext

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(nameToken)
}
