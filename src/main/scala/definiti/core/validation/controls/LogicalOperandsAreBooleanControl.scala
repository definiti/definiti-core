package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}

object LogicalOperandsAreBooleanControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper {
  override val description: String = "Check if all operands of logical expression are boolean expressions for AND and OR"
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
    logicalExpression.operator == LogicalOperator.Or || logicalExpression.operator == LogicalOperator.And
  }

  private def controlLogicalExpression(expression: LogicalExpression, library: Library): ControlResult = {
    (expression.left.returnType, expression.right.returnType) match {
      case (left, right) if isBoolean(left, library) && isBoolean(right, library) =>
        OK
      case (left, right) if isBoolean(left, library) =>
        ControlResult(errorNotBoolean(right, expression.right.location))
      case (left, right) if isBoolean(right, library) =>
        ControlResult(errorNotBoolean(left, expression.left.location))
      case (left, right) =>
        ControlResult(
          errorNotBoolean(left, expression.left.location),
          errorNotBoolean(right, expression.right.location)
        )
    }
  }

  def errorNotBoolean(returnType: AbstractTypeReference, location: Location): Alert = {
    alert(s"Boolean expected, got: ${returnType.readableString}", location)
  }
}
