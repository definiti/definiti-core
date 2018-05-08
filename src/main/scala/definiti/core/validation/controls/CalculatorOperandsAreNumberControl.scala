package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}

private[core] object CalculatorOperandsAreNumberControl extends Control[Root] with ExpressionControlHelper with TypeReferenceControlHelper {
  override val description: String = "Check if all operands of calculator expression are number expressions"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    testAllExpressions(library) { expression =>
      deepControl(expression) {
        case calculatorExpression: CalculatorExpression =>
          controlCalculatorExpression(calculatorExpression, library)
      }
    }
  }

  private def controlCalculatorExpression(expression: CalculatorExpression, library: Library): ControlResult = {
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
