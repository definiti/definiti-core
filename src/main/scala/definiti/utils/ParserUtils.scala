package definiti.utils

import definiti.{Position, Range}
import org.antlr.v4.runtime.ParserRuleContext

object ParserUtils {
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
    Range(
      Position(context.getStart.getLine, context.getStart.getCharPositionInLine),
      Position(context.getStop.getLine, context.getStop.getCharPositionInLine)
    )
  }
}
