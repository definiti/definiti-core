package definiti.core.mock.antlr

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.tree.{ParseTree, ParseTreeVisitor, TerminalNode}

case class TerminalNodeMock(token: Token) extends TerminalNode {
  override def getSymbol: Token = token

  override def getText: String = token.getText

  override def setParent(parent: RuleContext): Unit = ???

  override def toStringTree(parser: Parser): String = ???

  override def getParent: ParseTree = ???

  override def getChild(i: Int): ParseTree = ???

  override def accept[T](visitor: ParseTreeVisitor[_ <: T]): T = ???

  override def getSourceInterval: Interval = ???

  override def toStringTree: String = ???

  override def getChildCount: Int = ???

  override def getPayload: AnyRef = ???
}

object TerminalNodeMock {
  def apply(text: String) = new TerminalNodeMock(TokenMock(text))
}

case class TokenMock(
  text: String,
  line: Int,
  startIndex: Int,
  stopIndex: Int
) extends Token {
  override def getText: String = text

  override def getLine: Int = line

  override def getCharPositionInLine: Int = startIndex

  override def getStartIndex: Int = startIndex

  override def getStopIndex: Int = stopIndex

  override def getChannel: Int = ???

  override def getTokenIndex: Int = ???

  override def getTokenSource: TokenSource = ???

  override def getInputStream: CharStream = new ANTLRInputStream(text)

  override def getType: Int = ???
}

object TokenMock {
  def apply(text: String) = new TokenMock(text, 0, 0, 0)
}