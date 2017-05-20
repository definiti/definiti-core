package definiti.core.mock.antlr

import java.util

import definiti.core.parser.antlr.DefinitiParser.{VerifyingContext, _}
import definiti.core.utils.CollectionUtils._
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

case class VerifyingListContextMock(
  verifyingContexts: Seq[VerifyingContext]
) extends VerifyingListContext(null, 0) {
  override def verifying(): util.List[VerifyingContext] = javaList(verifyingContexts)

  override def verifying(i: Int): VerifyingContext = verifyingContexts(i)
}

case class VerifyingContextMock(
  verificationNameToken: Token,
  messageToken: Option[Token]
) extends VerifyingContext(null, 0) {
  this.verificationName = verificationNameToken
  this.message = messageToken.orNull

  override def IDENTIFIER(): TerminalNode = TerminalNodeMock(verificationNameToken)

  override def STRING(): TerminalNode = messageToken.map(TerminalNodeMock).orNull
}