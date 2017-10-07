package definiti.core.ast.pure

import definiti.core.ast.{CalculatorOperator, LogicalOperator, ParameterDefinition, Range, TypeReference}

private[core] sealed trait PureExpression {
  def range: Range
}

private[core] case class PureLogicalExpression(
  operator: LogicalOperator.Value,
  left: PureExpression,
  right: PureExpression,
  range: Range
) extends PureExpression

private[core] case class PureCalculatorExpression(
  operator: CalculatorOperator.Value,
  left: PureExpression,
  right: PureExpression,
  range: Range
) extends PureExpression

private[core] case class PureNot(inner: PureExpression, range: Range) extends PureExpression

private[core] case class PureBooleanValue(value: Boolean, range: Range) extends PureExpression

private[core] case class PureNumberValue(value: BigDecimal, range: Range) extends PureExpression

private[core] case class PureQuotedStringValue(value: String, range: Range) extends PureExpression

private[core] case class PureReference(name: String, range: Range) extends PureExpression

private[core] case class PureMethodCall(expression: PureExpression, method: String, parameters: Seq[PureExpression], generics: Seq[TypeReference], range: Range) extends PureExpression

private[core] case class PureAttributeCall(expression: PureExpression, attribute: String, range: Range) extends PureExpression

private[core] case class PureCombinedExpression(parts: Seq[PureExpression], range: Range) extends PureExpression

private[core] case class PureCondition(
  condition: PureExpression,
  onTrue: PureExpression,
  onFalse: Option[PureExpression],
  range: Range
) extends PureExpression

private[core] case class PureLambdaExpression(
  parameterList: Seq[ParameterDefinition],
  expression: PureExpression,
  range: Range
) extends PureExpression

private[core] case class PureFunctionCall(
  name: String,
  parameters: Seq[PureExpression],
  generics: Seq[TypeReference],
  range: Range
) extends PureExpression