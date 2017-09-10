package definiti.core.ast.pure

import definiti.core.ast.Range

sealed trait Expression {
  def range: Range
}

sealed trait LogicalExpression extends Expression

sealed trait CalculatorExpression extends Expression

case class Or(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class And(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class Equal(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class NotEqual(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class Lower(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class Upper(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class LowerOrEqual(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class UpperOrEqual(left: Expression, right: Expression, range: Range) extends LogicalExpression

case class Plus(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Minus(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Modulo(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Time(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Divide(left: Expression, right: Expression, range: Range) extends CalculatorExpression

case class Not(inner: Expression, range: Range) extends LogicalExpression

case class BooleanValue(value: Boolean, range: Range) extends LogicalExpression

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