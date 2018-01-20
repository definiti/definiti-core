package definiti.core.ast

sealed trait Expression {
  def location: Location

  def returnType: AbstractTypeReference
}

case class LogicalExpression(
  operator: LogicalOperator.Value,
  left: Expression,
  right: Expression,
  returnType: TypeReference,
  location: Location
) extends Expression

case class CalculatorExpression(
  operator: CalculatorOperator.Value,
  left: Expression,
  right: Expression,
  returnType: TypeReference,
  location: Location
) extends Expression

case class Not(inner: Expression, returnType: AbstractTypeReference, location: Location) extends Expression

sealed trait AtomicExpression extends Expression

case class BooleanValue(value: Boolean, returnType: AbstractTypeReference, location: Location) extends AtomicExpression

case class NumberValue(value: BigDecimal, returnType: AbstractTypeReference, location: Location) extends AtomicExpression

case class QuotedStringValue(value: String, returnType: AbstractTypeReference, location: Location) extends AtomicExpression

case class Reference(name: String, returnType: AbstractTypeReference, location: Location) extends AtomicExpression

case class MethodCall(expression: Expression, method: String, parameters: Seq[Expression], generics: Seq[TypeReference], returnType: AbstractTypeReference, location: Location) extends Expression

case class AttributeCall(expression: Expression, attribute: String, returnType: AbstractTypeReference, location: Location) extends Expression

case class CombinedExpression(parts: Seq[Expression], returnType: AbstractTypeReference, location: Location) extends Expression

case class Condition(
  condition: Expression,
  onTrue: Expression,
  onFalse: Option[Expression],
  returnType: AbstractTypeReference,
  location: Location
) extends Expression

case class LambdaExpression(
  parameterList: Seq[ParameterDefinition],
  expression: Expression,
  returnType: AbstractTypeReference,
  location: Location
) extends Expression

case class FunctionCall(
  name: String,
  parameters: Seq[Expression],
  generics: Seq[TypeReference],
  returnType: AbstractTypeReference,
  location: Location
) extends Expression

object LogicalOperator extends Enumeration {
  val Or, And, Equal, NotEqual, Lower, Upper, LowerOrEqual, UpperOrEqual = Value
}

object CalculatorOperator extends Enumeration {
  val Plus, Minus, Modulo, Time, Divide = Value
}