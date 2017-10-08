package definiti.core.ast.pure

import definiti.core.ast._

private[core] sealed trait PureExpression {
  def location: Location
}

private[core] case class PureLogicalExpression(
  operator: LogicalOperator.Value,
  left: PureExpression,
  right: PureExpression,
  location: Location
) extends PureExpression

private[core] case class PureCalculatorExpression(
  operator: CalculatorOperator.Value,
  left: PureExpression,
  right: PureExpression,
  location: Location
) extends PureExpression

private[core] case class PureNot(inner: PureExpression, location: Location) extends PureExpression

private[core] case class PureBooleanValue(value: Boolean, location: Location) extends PureExpression

private[core] case class PureNumberValue(value: BigDecimal, location: Location) extends PureExpression

private[core] case class PureQuotedStringValue(value: String, location: Location) extends PureExpression

private[core] case class PureReference(name: String, location: Location) extends PureExpression

private[core] case class PureMethodCall(expression: PureExpression, method: String, parameters: Seq[PureExpression], generics: Seq[TypeReference], location: Location) extends PureExpression

private[core] case class PureAttributeCall(expression: PureExpression, attribute: String, location: Location) extends PureExpression

private[core] case class PureCombinedExpression(parts: Seq[PureExpression], location: Location) extends PureExpression

private[core] case class PureCondition(
  condition: PureExpression,
  onTrue: PureExpression,
  onFalse: Option[PureExpression],
  location: Location
) extends PureExpression

private[core] case class PureLambdaExpression(
  parameterList: Seq[ParameterDefinition],
  expression: PureExpression,
  location: Location
) extends PureExpression

private[core] case class PureFunctionCall(
  name: String,
  parameters: Seq[PureExpression],
  generics: Seq[TypeReference],
  location: Location
) extends PureExpression