package definiti.core.utils

import definiti.core.{ParameterDefinition, Position, Range, TypeReference, Variable}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.TerminalNode

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

  def getRangeFromTerminalNode(terminalNode: TerminalNode): Range = {
    val symbol = terminalNode.getSymbol
    Range(
      Position(symbol.getLine, symbol.getStartIndex),
      Position(symbol.getLine, symbol.getStopIndex)
    )
  }

  def parametersToVariables(parameters: Seq[ParameterDefinition]) = {
    def variableParameter(parameter: ParameterDefinition) = parameter.typeReference match {
      case typeReference: TypeReference => typeReference
      case x => throw new RuntimeException(s"The variable could not have $x as type reference")
    }
    parameters.map(parameter => Variable(parameter.name, variableParameter(parameter), parameter.range))
  }
}
