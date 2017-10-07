package definiti.core.parser.project

import definiti.core.ast.{Position, Range, _}
import definiti.core.parser.antlr.DefinitiParser.{GenericTypeContext, GenericTypeListContext, ParameterDefinitionContext, ParameterListDefinitionContext}
import definiti.core.utils.CollectionUtils.scalaSeq
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.{ParserRuleContext, Token}

import scala.collection.mutable.ListBuffer

private[core] trait CommonParser {
  def processParameterListDefinition(context: ParameterListDefinitionContext): Seq[ParameterDefinition] = {
    scalaSeq(context.parameterDefinition()).map(processParameter)
  }

  def processParameter(context: ParameterDefinitionContext): ParameterDefinition = {
    ParameterDefinition(
      name = context.parameterName.getText,
      typeReference = TypeReference(context.parameterType.getText, processGenericTypeList(context.genericTypeList())),
      getRangeFromContext(context)
    )
  }

  def processGenericTypeList(context: GenericTypeListContext): Seq[TypeReference] = {
    if (context != null) {
      scalaSeq(context.genericType()).map(processGenericType)
    } else {
      Seq()
    }
  }

  def processGenericType(context: GenericTypeContext): TypeReference = {
    TypeReference(
      context.IDENTIFIER().getText,
      processGenericTypeList(context.genericTypeList())
    )
  }

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

  def getRangeFromToken(token: Token): Range = {
    Range(
      Position(token.getLine, token.getStartIndex),
      Position(token.getLine, token.getStopIndex)
    )
  }

  def appendIfDefined[A, B](element: A, buffer: ListBuffer[B], transformer: A => B): Unit = {
    if (element != null) {
      buffer.append(transformer(element))
    }
  }
}
