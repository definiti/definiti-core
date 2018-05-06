package definiti.core.parser.project

import definiti.core.ast.{Position, Range, _}
import definiti.core.parser.antlr.DefinitiParser._
import definiti.core.utils.CollectionUtils.scalaSeq
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{ParserRuleContext, Token}

import scala.collection.mutable.ListBuffer

private[core] trait CommonParser {
  def file: String

  def extractDocComment(string: String): String = {
    var temporaryResult = string
    if (temporaryResult.startsWith("/**")) {
      temporaryResult = temporaryResult.substring(3)
    }
    if (temporaryResult.endsWith("*/")) {
      temporaryResult = temporaryResult.substring(0, temporaryResult.length - 2)
    }
    temporaryResult
  }

  def getLocationFromContext(context: ParserRuleContext): Location = {
    Location(file, getRangeFromContext(context))
  }

  def getRangeFromContext(context: ParserRuleContext): Range = {
    val start = Option(context.getStart)
      .map(token => Position(token.getLine, token.getCharPositionInLine + 1))
      .getOrElse(Position.default)

    val end = Option(context.getStop)
      .map(token => Position(token.getLine, token.getCharPositionInLine + token.getText.length + 1))
      .getOrElse(Position.default)

    Range(start, end)
  }

  def getRangeFromTerminalNode(terminalNode: TerminalNode): Range = {
    val symbol = terminalNode.getSymbol
    Range(
      Position(symbol.getLine, symbol.getCharPositionInLine + 1),
      Position(symbol.getLine, symbol.getCharPositionInLine + symbol.getText.length + 1)
    )
  }

  def getRangeFromToken(token: Token): Range = {
    Range(
      Position(token.getLine, token.getCharPositionInLine + 1),
      Position(token.getLine, token.getCharPositionInLine + token.getText.length + 1)
    )
  }

  def appendIfDefined[A, B](element: A, buffer: ListBuffer[B], transformer: A => B): Unit = {
    if (element != null) {
      buffer.append(transformer(element))
    }
  }
}
