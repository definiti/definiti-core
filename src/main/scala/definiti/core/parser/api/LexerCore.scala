package definiti.core.parser.api

import definiti.common.ast.Location
import definiti.common.validation._

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{OffsetPosition, Positional}

class LexerCore(filename: String) extends RegexParsers {
  def parse(value: String): Validated[Seq[TokenCore]] = {
    parse(tokens, value) match {
      case Success(result, _) =>
        val resultWithoutComment = result
          .filterNot(_.isInstanceOf[LINE_COMMENT])
          .filterNot(_.isInstanceOf[MULTILINE_COMMENT])
        Valid(resultWithoutComment)

      case Failure(message, next) =>
        Invalid(
          message = message,
          location = Location(filename, 0, 0, next.pos.line, next.pos.column)
        )

      case Error(message, next) =>
        Invalid(
          message = message,
          location = Location(filename, 0, 0, next.pos.line, next.pos.column)
        )
    }
  }

  def tokens: Parser[Seq[TokenCore]] = {
    phrase(rep1(
      token("type", TYPE)
        | token("[", LEFT_BRACKET)
        | token("]", RIGHT_BRACKET)
        | token("{", LEFT_BRACE)
        | token("}", RIGHT_BRACE)
        | token("(", LEFT_PARENTHESIS)
        | token(")", RIGHT_PARENTHESIS)
        | token(":", COLON)
        | token(",", COMMA)
        | token("=>", RIGHT_ARROW)
        | variableToken("[a-zA-Z0-9]+".r, IDENTIFIER)
        | variableToken("\\/\\*\\*+[^*]*\\*+(?:[^/*][^*]*\\*+)*\\/".r, DOC_COMMENT)
        | variableToken("/\\*+[^*]*\\*+(?:[^/*][^*]*\\*+)*/".r, MULTILINE_COMMENT)
        | variableToken("\\/\\/[\\w\\s\\']*".r, LINE_COMMENT)
    ))
  }

  def token[T <: TokenCore](keywordOrSymbol: String, constructor: () => T): Parser[T] = {
    positioned {
      keywordOrSymbol ^^ (_ => constructor())
    } map { token =>
      token.posEnd = token.pos match {
        case offsetPosition: OffsetPosition => offsetPosition.copy(offset = offsetPosition.offset + keywordOrSymbol.length)
        case position => position
      }
      token
    }
  }

  case class VariableWrapper[T <: TokenCore](token: T, content: String) extends Positional

  def variableToken[T <: TokenCore](regex: Parser[String], constructor: String => T): Parser[T] = {
    positioned {
      regex ^^ (value => VariableWrapper(constructor(value), value))
    } map { variableWrapper =>
      variableWrapper.token.pos = variableWrapper.pos
      variableWrapper.token.posEnd = variableWrapper.pos match {
        case offsetPosition: OffsetPosition => offsetPosition.copy(offset = offsetPosition.offset + variableWrapper.content.length)
        case position => position
      }
      variableWrapper.token
    }
  }
}
