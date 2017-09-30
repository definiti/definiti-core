package definiti.core.ast.typed

import definiti.core.ast.Range
import definiti.core.ast.pure._

sealed trait Expression {
  def range: Range

  def returnType: AbstractTypeReference
}

case class LogicalExpression(
  operator: LogicalOperator.Value,
  left: Expression,
  right: Expression,
  returnType: TypeReference,
  range: Range
) extends Expression

case class CalculatorExpression(
  operator: CalculatorOperator.Value,
  left: Expression,
  right: Expression,
  returnType: TypeReference,
  range: Range
) extends Expression

case class Not(inner: Expression, returnType: AbstractTypeReference, range: Range) extends Expression

case class BooleanValue(value: Boolean, returnType: AbstractTypeReference, range: Range) extends Expression

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