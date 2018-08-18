package definiti.core.parser.project

import definiti.common.ast.{CalculatorOperator, LogicalOperator}

import scala.util.parsing.input.{NoPosition, Position, Positional}

sealed trait TokenProject extends Positional {
  var posEnd: Position = NoPosition
}

case class PACKAGE() extends TokenProject
case class IMPORT() extends TokenProject
case class TYPE() extends TokenProject
case class IF() extends TokenProject
case class ELSE() extends TokenProject
case class VERIFICATION() extends TokenProject
case class VERIFY() extends TokenProject
case class VERIFYING() extends TokenProject
case class DEF() extends TokenProject
case class CONTEXT() extends TokenProject
case class ENUM() extends TokenProject
case class MESSAGE() extends TokenProject
case class OK() extends TokenProject
case class KO() extends TokenProject
case class AS() extends TokenProject

case class LEFT_BRACKET() extends TokenProject
case class RIGHT_BRACKET() extends TokenProject
case class LEFT_BRACE() extends TokenProject
case class RIGHT_BRACE() extends TokenProject
case class LEFT_PARENTHESIS() extends TokenProject
case class RIGHT_PARENTHESIS() extends TokenProject
case class COLON() extends TokenProject
case class COMMA() extends TokenProject
case class RIGHT_ARROW() extends TokenProject
case class DOT() extends TokenProject
case class EQUAL() extends TokenProject
case class QUESTION() extends TokenProject

case class BOOLEAN(value: Boolean) extends TokenProject
case class NUMBER(value: BigDecimal) extends TokenProject
case class INTEGER(value: BigInt) extends TokenProject
case class STRING(value: String) extends TokenProject
case class IDENTIFIER(value: String) extends TokenProject
case class CALCULATOR_OPERATOR_LEVEL_1(kind: CalculatorOperator.Value) extends TokenProject
case class CALCULATOR_OPERATOR_LEVEL_2(kind: CalculatorOperator.Value) extends TokenProject
case class LOGICAL_OPERATOR(kind: LogicalOperator.Value) extends TokenProject
case class LOGICAL_COMBINATION_OPERATOR(kind: LogicalOperator.Value) extends TokenProject
case class NOT() extends TokenProject

case class DOC_COMMENT(value: String) extends TokenProject
case class MULTILINE_COMMENT(value: String) extends TokenProject
case class LINE_COMMENT(value: String) extends TokenProject

case class CONTEXT_CONTENT(content: String) extends TokenProject