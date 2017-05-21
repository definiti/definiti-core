package definiti.core.utils

import definiti.core.{ParameterDefinition, Position, Range, TypeReference}
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{ParserRuleContext, Token}

private[core] object ParserUtils {
  def extractStringContent(string: String): String = {
    var temporaryResult = string
    if (temporaryResult.startsWith("\"")) {
      temporaryResult = temporaryResult.substring(1)
    }
    if (temporaryResult.endsWith("\"")) {
      temporaryResult = temporaryResult.substring(0, temporaryResult.length - 1)
    }
    temporaryResult
  }

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

  def getRangeFromContext(context: ParserRuleContext): Range = {
    def position(token: Token): Position = {
      if (token == null) {
        Position(0, 0)
      } else {
        Position(token.getLine, token.getCharPositionInLine)
      }
    }

    Range(position(context.getStart), position(context.getStop))
  }

  def getRangeFromTerminalNode(terminalNode: TerminalNode): Range = {
    val symbol = terminalNode.getSymbol
    Range(
      Position(symbol.getLine, symbol.getStartIndex),
      Position(symbol.getLine, symbol.getStopIndex)
    )
  }
}
