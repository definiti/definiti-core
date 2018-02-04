package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}

object EqualityOnSameTypeControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper {
  override val description: String = "Check if both expressions on equality comparison have the same type"
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
    logicalExpression.operator == LogicalOperator.Equal || logicalExpression.operator == LogicalOperator.NotEqual
  }

  private def controlLogicalExpression(expression: LogicalExpression, library: Library): ControlResult = {
    if (expression.left.returnType == expression.right.returnType) {
      OK
    } else {
      ControlResult(
        errorDifferentTypes(expression.left.returnType, expression.right.returnType, expression.location)
      )
    }
  }

  def errorDifferentTypes(left: AbstractTypeReference, right: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"Both left and right expressions must be the same type (left: ${left.readableString}, right: ${right.readableString})",
      location
    )
  }
}
