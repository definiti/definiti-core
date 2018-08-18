package definiti.core.parser.project

import definiti.common.ast.{CalculatorOperator, Location, LogicalOperator}
import definiti.common.validation.{Invalid, Valid, Validated}

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{OffsetPosition, Positional}

class LexerProject(filename: String) extends RegexParsers {
  def parse(value: String): Validated[Seq[TokenProject]] = {
    parse(tokens, value) match {
      case Success(result, _) =>
        val resultWithoutComment = result
          .filterNot(_.isInstanceOf[LINE_COMMENT])
          .filterNot(_.isInstanceOf[MULTILINE_COMMENT])
        Valid(resultWithoutComment)

      case Failure(message, next) =>
        Invalid(
          message = message,
          location = Location(filename, next.pos.line, next.pos.column, next.pos.line, next.pos.column)
        )

      case Error(message, next) =>
        Invalid(
          message = message,
          location = Location(filename, next.pos.line, next.pos.column, next.pos.line, next.pos.column)
        )
    }
  }

  def tokens: Parser[Seq[TokenProject]] = {
    phrase(rep1(
      variableToken("(?s)\\{\\{\\{(.*)(?!\\}\\}\\})(.*)\\}\\}\\}".r, x => CONTEXT_CONTENT(x.substring(3, x.length - 3)))
        | variableToken("\\/\\*\\*+[^*]*\\*+(?:[^/*][^*]*\\*+)*\\/".r, DOC_COMMENT)
        | variableToken("/\\*+[^*]*\\*+(?:[^/*][^*]*\\*+)*/".r, MULTILINE_COMMENT)
        | variableToken("\\/\\/[^\\n]*".r, LINE_COMMENT)

        | token("package", PACKAGE)
        | token("import", IMPORT)
        | token("type", TYPE)
        | token("if", IF)
        | token("else", ELSE)
        | token("verification", VERIFICATION)
        | token("verifying", VERIFYING)
        | token("verify", VERIFY)
        | token("def", DEF)
        | token("context", CONTEXT)
        | token("enum", ENUM)
        | token("message", MESSAGE)
        | token("ok", OK)
        | token("ko", KO)
        | token("as", AS)

        | symbol("[", LEFT_BRACKET)
        | symbol("]", RIGHT_BRACKET)
        | symbol("{", LEFT_BRACE)
        | symbol("}", RIGHT_BRACE)
        | symbol("(", LEFT_PARENTHESIS)
        | symbol(")", RIGHT_PARENTHESIS)
        | symbol(":", COLON)
        | symbol(",", COMMA)
        | symbol(".", DOT)
        | symbol("?", QUESTION)

        | symbol("==", () => LOGICAL_OPERATOR(LogicalOperator.Equal))
        | symbol("!=", () => LOGICAL_OPERATOR(LogicalOperator.NotEqual))
        | symbol("<=", () => LOGICAL_OPERATOR(LogicalOperator.LowerOrEqual))
        | symbol("<", () => LOGICAL_OPERATOR(LogicalOperator.Lower))
        | symbol(">=", () => LOGICAL_OPERATOR(LogicalOperator.UpperOrEqual))
        | symbol(">", () => LOGICAL_OPERATOR(LogicalOperator.Upper))

        | symbol("=>", RIGHT_ARROW)
        | symbol("=", EQUAL)

        | symbol("&&", () => LOGICAL_COMBINATION_OPERATOR(LogicalOperator.And))
        | symbol("||", () => LOGICAL_COMBINATION_OPERATOR(LogicalOperator.Or))
        | symbol("!", NOT)

        | variableToken("true\\b|false\\b".r, x => BOOLEAN(x == "true"))
        | variableToken("[0-9]+\\.[0-9]+".r, x => NUMBER(BigDecimal(x)))
        | variableToken("[0-9]+".r, x => INTEGER(BigInt(x)))
        | variableToken(""" "(\"|.)*" """.trim.r, x => STRING(x.substring(1, x.length - 1)))
        | variableToken("[a-zA-Z0-9]+".r, IDENTIFIER)

        | symbol("*", () => CALCULATOR_OPERATOR_LEVEL_1(CalculatorOperator.Time))
        | symbol("/", () => CALCULATOR_OPERATOR_LEVEL_1(CalculatorOperator.Divide))
        | symbol("%", () => CALCULATOR_OPERATOR_LEVEL_1(CalculatorOperator.Modulo))
        | symbol("+", () => CALCULATOR_OPERATOR_LEVEL_2(CalculatorOperator.Plus))
        | symbol("-", () => CALCULATOR_OPERATOR_LEVEL_2(CalculatorOperator.Minus))
    ))
  }

  def token[T <: TokenProject](keyword: String, constructor: () => T): Parser[T] = {
    positioned {
      s"${keyword}\\b".r ^^ (_ => constructor())
    } map { token =>
      token.posEnd = token.pos match {
        case offsetPosition: OffsetPosition => offsetPosition.copy(offset = offsetPosition.offset + keyword.length)
        case position => position
      }
      token
    }
  }

  def symbol[T <: TokenProject](symbole: String, constructor: () => T): Parser[T] = {
    positioned {
      symbole ^^ (_ => constructor())
    } map { token =>
      token.posEnd = token.pos match {
        case offsetPosition: OffsetPosition => offsetPosition.copy(offset = offsetPosition.offset + symbole.length)
        case position => position
      }
      token
    }
  }

  case class VariableWrapper[T <: TokenProject](token: T, content: String) extends Positional

  def variableToken[T <: TokenProject](regex: Parser[String], constructor: String => T): Parser[T] = {
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
