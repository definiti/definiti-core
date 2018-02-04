package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}

object NotExpressionIsBooleanControl extends Control with TypeReferenceControlHelper with ExpressionControlHelper {
  override val description: String = "Check if not expression is a boolean expression"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

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
    alert(s"Boolean expected, got: ${returnType.readableString}", location)
  }
}
