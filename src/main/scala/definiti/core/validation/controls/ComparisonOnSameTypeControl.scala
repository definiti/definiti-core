package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}

private[core] object ComparisonOnSameTypeControl extends Control[Root] with ExpressionControlHelper with TypeReferenceControlHelper {
  override val description: String = "Check if both expressions on equality comparison have the same type"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  private val acceptedOperators = {
    import definiti.common.ast.LogicalOperator._
    Seq(Equal, NotEqual, Lower, Upper, LowerOrEqual, UpperOrEqual)
  }

  override def control(root: Root, library: Library): ControlResult = {
    testAllExpressions(library) { expression =>
      deepControl(expression) {
        case logicalExpression: LogicalExpression if acceptedOperators.contains(logicalExpression.operator) =>
          controlLogicalExpression(logicalExpression, library)
      }
    }
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
