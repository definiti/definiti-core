package definiti.core.ast.pure

import definiti.core.ast.Range

sealed trait Expression {
  def range: Range
}

case class LogicalExpression(
  operator: LogicalOperator.Value,
  left: Expression,
  right: Expression,
  range: Range
) extends Expression

object LogicalOperator extends Enumeration {
  val Or, And, Equal, NotEqual, Lower, Upper, LowerOrEqual, UpperOrEqual = Value
}

case class CalculatorExpression(
  operator: CalculatorOperator.Value,
  left: Expression,
  right: Expression,
  range: Range
) extends Expression

object CalculatorOperator extends Enumeration {
  val Plus, Minus, Modulo, Time, Divide = Value
}

case class Not(inner: Expression, range: Range) extends Expression

case class BooleanValue(value: Boolean, range: Range) extends Expression

case class NumberValue(value: BigDecimal, range: Range) extends Expression

case class QuotedStringValue(value: String, range: Range) extends Expression

case class Reference(name: String, range: Range) extends Expression

case class MethodCall(expression: Expression, method: String, parameters: Seq[Expression], generics: Seq[TypeReference], range: Range) extends Expression

case class AttributeCall(expression: Expression, attribute: String, range: Range) extends Expression

case class CombinedExpression(parts: Seq[Expression], range: Range) extends Expression

case class Condition(
  condition: Expression,
  onTrue: Expression,
  onFalse: Option[Expression],
  range: Range
) extends Expression

case class LambdaExpression(
  parameterList: Seq[ParameterDefinition],
  expression: Expression,
  range: Range
) extends Expression

case class FunctionCall(
  name: String,
  parameters: Seq[Expression],
  generics: Seq[TypeReference],
  range: Range
) extends Expression