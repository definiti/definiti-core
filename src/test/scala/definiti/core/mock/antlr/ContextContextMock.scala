package definiti.core.mock.antlr

import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils._
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

case class ContextContextMock(
  identifier: TerminalNode,
  contextContentContext: ContextContentContext
) extends ContextContext(null, 0) {
  override def IDENTIFIER: TerminalNode = identifier

  override def contextContent: ContextContentContext = contextContentContext
}

case class ContextContentMock(
  contextContentSymbolContextSeq: Seq[ContextContentSymbolContext]
) extends ContextContentContext(null, 0) {
  override def contextContentSymbol() = javaList(contextContentSymbolContextSeq)

  override def contextContentSymbol(i: Int) = contextContentSymbolContextSeq(i)

  // This trick is to avoid OutOfBoundException.
  // Testing with Antlr is not quite simple and it starts to be quite complex.
  override def getStart: Token = {
    val text = contextContentSymbolContextSeq.map(_.getText).mkString("")
    new TokenMock(
      text = text,
      line = 0,
      startIndex = 0,
      stopIndex = text.length
    )
  }

  // This trick is to avoid OutOfBoundException.
  // Testing with Antlr is not quite simple and it starts to be quite complex.
  override def getStop: Token = {
    val text = contextContentSymbolContextSeq.map(_.getText).mkString("")
    new TokenMock(
      text = text,
      line = 0,
      startIndex = 0,
      stopIndex = text.length
    )
  }
}

object ContextContentSymbolContextMock extends ContextContentSymbolContext(null, 0)