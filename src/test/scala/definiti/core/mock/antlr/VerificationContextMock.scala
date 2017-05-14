package definiti.core.mock.antlr

import definiti.core.parser.antlr.DefinitiParser._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class VerificationContextMock(
  verificationNameToken: Token,
  verificationMessageToken: Token,
  functionContext: FunctionContext,
  identifier: TerminalNode,
  string: TerminalNode,
  docComment: TerminalNode
) extends VerificationContext(null, 0) {
  this.verificationName = verificationNameToken

  this.verificationMessage = verificationMessageToken

  override def function(): FunctionContext = functionContext

  override def IDENTIFIER(): TerminalNode = identifier

  override def STRING(): TerminalNode = string

  override def DOC_COMMENT(): TerminalNode = docComment
}