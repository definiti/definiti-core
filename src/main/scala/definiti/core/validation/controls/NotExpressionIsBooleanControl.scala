package definiti.core.validation.controls

import definiti.core.ast._
import definiti.core.validation.controls.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}
import definiti.core.{Alert, AlertControl}

object NotExpressionIsBooleanControl extends Control with TypeReferenceControlHelper with ExpressionControlHelper {
  override def name: String = "notExpressionIsBoolean"

  override def description: String = "Check if not expression is a boolean expression"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    testAllExpressions(library) { expression =>
      deepControl(expression) {
        case not: Not =>
          controlNotExpression(not, library)
      }
    }
  }

  private def controlNotExpression(expression: Not, library: Library): ControlResult = {
    if (isBoolean(expression.inner.returnType, library)) {
      OK
    } else {
      ControlResult(errorNotBoolean(expression.inner.returnType, expression.inner.location))
    }
  }

  def errorNotBoolean(returnType: AbstractTypeReference, location: Location): Alert = {
    AlertControl(
      name,
      s"Boolean expected, got: ${returnType.readableString}",
      location
    )
  }
}
