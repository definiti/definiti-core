package definiti.core.parser.project

import definiti.common.ast.{Range, TypeReference, Position => ASTPosition}

import scala.util.parsing.input.Position

trait ProjectParserHelper {
  self: DefinitiFileParser =>

  val unset = TypeReference("_UNSET_", Seq.empty)

  def `package`: Parser[PACKAGE] = accept("package", { case x: PACKAGE => x })
  def `import`: Parser[IMPORT] = accept("import", { case x: IMPORT => x })
  def `type`: Parser[TYPE] = accept("type", { case x: TYPE => x })
  def `if`: Parser[IF] = accept("if", { case x: IF => x })
  def `else`: Parser[ELSE] = accept("else", { case x: ELSE => x })
  def `verification`: Parser[VERIFICATION] = accept("verification", { case x: VERIFICATION => x })
  def `verify`: Parser[VERIFY] = accept("verify", { case x: VERIFY => x })
  def `verifying`: Parser[VERIFYING] = accept("verifying", { case x: VERIFYING => x })
  def `def`: Parser[DEF] = accept("def", { case x: DEF => x })
  def `context`: Parser[CONTEXT] = accept("context", { case x: CONTEXT => x })
  def `enum`: Parser[ENUM] = accept("enum", { case x: ENUM => x })
  def `message`: Parser[MESSAGE] = accept("message", { case x: MESSAGE => x })
  def `ok`: Parser[OK] = accept("ok", { case x: OK => x })
  def `ko`: Parser[KO] = accept("ko", { case x: KO => x })
  def `as`: Parser[AS] = accept("as", { case x: AS => x })

  def `[`: Parser[LEFT_BRACKET] = accept("[", { case x: LEFT_BRACKET => x })
  def `]`: Parser[RIGHT_BRACKET] = accept("]", { case x: RIGHT_BRACKET => x })
  def `{`: Parser[LEFT_BRACE] = accept("{", { case x: LEFT_BRACE => x })
  def `}`: Parser[RIGHT_BRACE] = accept("}", { case x: RIGHT_BRACE => x })
  def `(`: Parser[LEFT_PARENTHESIS] = accept("(", { case x: LEFT_PARENTHESIS => x })
  def `)`: Parser[RIGHT_PARENTHESIS] = accept(")", { case x: RIGHT_PARENTHESIS => x })
  def `:`: Parser[COLON] = accept(":", { case x: COLON => x })
  def `,`: Parser[COMMA] = accept(",", { case x: COMMA => x })
  def `=>`: Parser[RIGHT_ARROW] = accept("=>", { case x: RIGHT_ARROW => x })
  def `.`: Parser[DOT] = accept(".", { case x: DOT => x })
  def `=`: Parser[EQUAL] = accept("=", { case x: EQUAL => x })
  def `?`: Parser[QUESTION] = accept("?", { case x: QUESTION => x })

  def boolean: Parser[BOOLEAN] = accept("boolean", { case x: BOOLEAN => x })
  def number: Parser[NUMBER] = accept("number", { case x: NUMBER => x })
  def integer: Parser[INTEGER] = accept("integer", { case x: INTEGER => x })
  def string: Parser[STRING] = accept("string", { case x: STRING => x })
  def identifier: Parser[IDENTIFIER] = accept("identifier", { case x: IDENTIFIER => x })
  def calculatorOperatorLevel1: Parser[CALCULATOR_OPERATOR_LEVEL_1] = accept("calculatorOperatorLevel1", { case x: CALCULATOR_OPERATOR_LEVEL_1 => x })
  def calculatorOperatorLevel2: Parser[CALCULATOR_OPERATOR_LEVEL_2] = accept("calculatorOperatorLevel2", { case x: CALCULATOR_OPERATOR_LEVEL_2 => x })
  def logicalOperator: Parser[LOGICAL_OPERATOR] = accept("logicalOperator", { case x: LOGICAL_OPERATOR => x })
  def logicalCombinationOperator: Parser[LOGICAL_COMBINATION_OPERATOR] = accept("logicalCombinationOperator", { case x: LOGICAL_COMBINATION_OPERATOR => x })
  def not: Parser[NOT] = accept("not", { case x: NOT => x })

  def docComment: Parser[DOC_COMMENT] = accept("docComment", { case x: DOC_COMMENT => x })

  def contextContent: Parser[CONTEXT_CONTENT] = accept("contextContent", { case x: CONTEXT_CONTENT => x })

  def dottedIdentifier: Parser[String] = {
    joinedElements(identifier, DOT())
      .map(identifiers => identifiers.map(_.value))
      .map(_.mkString("."))
  }

  def rangedDottedIdentifier: Parser[Ranged[String]] = {
    joinedElements(identifier, DOT())
      .map { identifiers =>
        Ranged(
          value = identifiers.map(_.value).mkString("."),
          range = range(identifiers.head, identifiers.last)
        )
      }
  }

  def joinedElements[A, B <: TokenProject](elementParser: Parser[A], separatorParser: Parser[B]): Parser[Seq[A]] = {
    (elementParser ~ separatorParser).* ~ elementParser ^^ {
      case elementSeparatorSeq ~ lastElement =>
        val previousElements = elementSeparatorSeq.map {
          case innerElement ~ _ => innerElement
        }
        previousElements :+ lastElement
    }
  }

  def container[OPEN <: TokenProject, CONTENT, CLOSE <: TokenProject](openingToken: Parser[OPEN], content: Parser[CONTENT], closingToken: Parser[CLOSE]): Parser[CONTENT] = {
    (openingToken ~ content ~ closingToken) ^^ {
      case _ ~ containerContent ~ _ => containerContent
    }
  }

  def rangedContainer[OPEN <: TokenProject, CONTENT, CLOSE <: TokenProject](openingToken: Parser[OPEN], content: Parser[CONTENT], closingToken: Parser[CLOSE]): Parser[Ranged[CONTENT]] = {
    (openingToken ~ content ~ closingToken) ^^ {
      case firstToken ~ containerContent ~ lastToken =>
        Ranged(
          value = containerContent,
          range = range(firstToken, lastToken)
        )
    }
  }

  def range[FIRST <: TokenProject, LAST <: TokenProject](firstToken: FIRST, lastToken: LAST): Range = {
    Range(
      position(firstToken.pos),
      position(lastToken.posEnd)
    )
  }

  def range[FIRST <: TokenProject](firstToken: FIRST, lastToken: Ranged[_]): Range = {
    Range(
      position(firstToken.pos),
      lastToken.range.end
    )
  }

  def range[LAST <: TokenProject](firstToken: Ranged[_], lastToken: LAST): Range = {
    Range(
      firstToken.range.start,
      position(lastToken.posEnd)
    )
  }

  def range[LAST <: TokenProject](firstToken: Ranged[_], lastToken: Ranged[_]): Range = {
    Range(
      firstToken.range.start,
      lastToken.range.end
    )
  }

  def position(position: Position): ASTPosition = {
    ASTPosition(
      line = position.line,
      column = position.column
    )
  }

  case class Ranged[A](value: A, range: Range)

}
