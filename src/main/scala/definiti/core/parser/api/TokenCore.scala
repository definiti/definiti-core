package definiti.core.parser.api

import scala.util.parsing.input.{NoPosition, Position, Positional}

sealed trait TokenCore extends Positional {
  var posEnd: Position = NoPosition
}

case class IDENTIFIER(value: String) extends TokenCore

case class TYPE() extends TokenCore

case class LEFT_BRACKET() extends TokenCore

case class RIGHT_BRACKET() extends TokenCore

case class LEFT_BRACE() extends TokenCore

case class RIGHT_BRACE() extends TokenCore

case class LEFT_PARENTHESIS() extends TokenCore

case class RIGHT_PARENTHESIS() extends TokenCore

case class COLON() extends TokenCore

case class COMMA() extends TokenCore

case class RIGHT_ARROW() extends TokenCore

case class DOC_COMMENT(value: String) extends TokenCore

case class MULTILINE_COMMENT(value: String) extends TokenCore

case class LINE_COMMENT(value: String) extends TokenCore