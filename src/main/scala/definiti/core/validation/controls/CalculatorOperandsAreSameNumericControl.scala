package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.{ExpressionControlHelper, TypeReferenceControlHelper}

private[core] object CalculatorOperandsAreSameNumericControl extends Control[Root] with ExpressionControlHelper with TypeReferenceControlHelper {
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
    val leftIsNumber = isNumber(expression.left.returnType, library)
    val rightIsNumber = isNumber(expression.right.returnType, library)

    val leftIsInteger = isInteger(expression.left.returnType, library)
    val rightIsInteger = isInteger(expression.right.returnType, library)

    val leftIsNumeric = leftIsNumber || leftIsInteger
    val rightIsNumeric = rightIsNumber || rightIsInteger

    if (leftIsNumeric && rightIsNumeric) {
      if (leftIsNumber && rightIsNumber || leftIsInteger && rightIsInteger) {
        OK
      } else {
        ControlResult(differentNumeric(expression.left.returnType, expression.right.returnType, expression.location))
      }
    } else if (leftIsNumeric) {
      ControlResult(errorNotNumeric(expression.right.returnType, expression.right.location))
    } else if (rightIsNumeric) {
      ControlResult(errorNotNumeric(expression.left.returnType, expression.left.location))
    } else {
      ControlResult(
        errorNotNumeric(expression.left.returnType, expression.left.location),
        errorNotNumeric(expression.right.returnType, expression.right.location)
      )
    }
  }

  def errorNotNumeric(returnType: AbstractTypeReference, location: Location): Alert = {
    alert(s"Numeric expected, got: ${returnType.readableString}", location)
  }

  def differentNumeric(left: AbstractTypeReference, right: AbstractTypeReference, location: Location): Alert = {
    alert(s"Numeric types are different: ${left.readableString} and ${right.readableString}", location)
  }
}
