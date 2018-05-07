package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}

private[core] object OrderOperandsAreNumberControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper {
  override val description: String = "Check if all operands of logical expression are number expressions for >, <, >= and <="
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    testAllExpressions(library) { expression =>
      deepControl(expression) {
        case logicalExpression: LogicalExpression if shouldBeControlled(logicalExpression) =>
          controlLogicalExpression(logicalExpression, library)
      }
    }
  }

  private def shouldBeControlled(logicalExpression: LogicalExpression): Boolean = {
    logicalExpression.operator == LogicalOperator.Lower ||
      logicalExpression.operator == LogicalOperator.Upper ||
      logicalExpression.operator == LogicalOperator.LowerOrEqual ||
      logicalExpression.operator == LogicalOperator.UpperOrEqual
  }

  private def controlLogicalExpression(expression: LogicalExpression, library: Library): ControlResult = {
    (expression.left.returnType, expression.right.returnType) match {
      case (left, right) if isNumber(left, library) && isNumber(right, library) =>
        OK
      case (left, right) if isNumber(left, library) =>
        ControlResult(errorNotNumber(right, expression.right.location))
      case (left, right) if isNumber(right, library) =>
        ControlResult(errorNotNumber(left, expression.left.location))
      case (left, right) =>
        ControlResult(
          errorNotNumber(left, expression.left.location),
          errorNotNumber(right, expression.right.location)
        )
    }
  }

  def errorNotNumber(returnType: AbstractTypeReference, location: Location): Alert = {
    alert(s"Number expected, got: ${returnType.readableString}", location)
  }
}
