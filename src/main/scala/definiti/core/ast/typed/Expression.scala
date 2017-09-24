package definiti.core.ast.typed

import definiti.core.ast.Range
import definiti.core.ast.pure.{AbstractTypeReference, ParameterDefinition, TypeReference}

sealed trait Expression {
  def range: Range

  def returnType: AbstractTypeReference
}

sealed trait LogicalExpression extends Expression

sealed trait CalculatorExpression extends Expression

case class Or(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class And(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class Equal(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class NotEqual(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class Lower(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class Upper(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class LowerOrEqual(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class UpperOrEqual(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class Plus(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends CalculatorExpression

case class Minus(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends CalculatorExpression

case class Modulo(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends CalculatorExpression

case class Time(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends CalculatorExpression

case class Divide(left: Expression, right: Expression, returnType: AbstractTypeReference, range: Range) extends CalculatorExpression

case class Not(inner: Expression, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class BooleanValue(value: Boolean, returnType: AbstractTypeReference, range: Range) extends LogicalExpression

case class NumberValue(value: BigDecimal, returnType: AbstractTypeReference, range: Range) extends Expression

case class QuotedStringValue(value: String, returnType: AbstractTypeReference, range: Range) extends Expression

case class Reference(name: String, returnType: AbstractTypeReference, range: Range) extends Expression

case class MethodCall(expression: Expression, method: String, parameters: Seq[Expression], generics: Seq[TypeReference], returnType: AbstractTypeReference, range: Range) extends Expression

case class AttributeCall(expression: Expression, attribute: String, returnType: AbstractTypeReference, range: Range) extends Expression

case class CombinedExpression(parts: Seq[Expression], returnType: AbstractTypeReference, range: Range) extends Expression

case class Condition(
  condition: Expression,
  onTrue: Expression,
  onFalse: Option[Expression],
  returnType: AbstractTypeReference,
  range: Range
) extends Expression

case class LambdaExpression(
  parameterList: Seq[ParameterDefinition],
  expression: Expression,
  returnType: AbstractTypeReference,
  range: Range
) extends Expression

case class FunctionCall(
  name: String,
  parameters: Seq[Expression],
  generics: Seq[TypeReference],
  returnType: AbstractTypeReference,
  range: Range
) extends Expression