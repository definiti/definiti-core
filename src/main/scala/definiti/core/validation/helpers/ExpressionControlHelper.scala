package definiti.core.validation.helpers

import definiti.common.ast._
import definiti.common.control.{Control, ControlResult}

private[core] trait ExpressionControlHelper {
  self: Control[Root] =>

  def deepControl(expression: Expression)(op: PartialFunction[Expression, ControlResult]): ControlResult = {
    def deepProcess(expression: Expression): ControlResult = {
      val deepControl = expression match {
        case methodCall: MethodCall =>
          deepProcess(methodCall.expression)

        case attributeCall: AttributeCall =>
          deepProcess(attributeCall.expression)

        case combinedExpression: CombinedExpression =>
          ControlResult.squash(combinedExpression.parts.map(deepProcess))

        case condition: Condition =>
          ControlResult.squash {
            Seq(
              deepProcess(condition.condition),
              deepProcess(condition.onTrue)
            ) ++ condition.onFalse.map(deepProcess)
          }

        case logicalExpression: LogicalExpression =>
          ControlResult.squash {
            Seq(
              deepProcess(logicalExpression.left),
              deepProcess(logicalExpression.right)
            )
          }

        case calculatorExpression: CalculatorExpression =>
          ControlResult.squash {
            Seq(
              deepProcess(calculatorExpression.left),
              deepProcess(calculatorExpression.right)
            )
          }

        case _ => OK
      }
      val directControl = op.applyOrElse(expression, (_: Expression) => OK)
      ControlResult.squash(Seq(deepControl, directControl))
    }

    deepProcess(expression)
  }

  def testAllExpressions(library: Library)(control: Expression => ControlResult): ControlResult = {
    val namedFunctionControl = library.namedFunctions.map(_.body).map(control)
    val verificationControl = library.verifications.map(_.function.body).map(control)
    val aliasTypeVerificationControl = library.aliasTypes.flatMap(_.verifications).map(_.function.body).map(control)
    val definedTypeVerificationControl = library.definedTypes.flatMap(_.verifications).map(_.function.body).map(control)

    ControlResult.squash {
      namedFunctionControl ++
        verificationControl ++
        aliasTypeVerificationControl ++
        definedTypeVerificationControl
    }
  }
}
