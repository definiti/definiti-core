package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.{ExpressionControlHelper, ParameterControlHelper, TypeReferenceControlHelper}

private[core] object FunctionParametersControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper with ParameterControlHelper {
  override val description: String = "Check if parameters and arguments on a named function are the same"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    testAllExpressions(library) { expression =>
      deepControl(expression) {
        case functionCall: FunctionCall => controlFunctionCall(functionCall, library)
      }
    }
  }

  private def controlFunctionCall(functionCall: FunctionCall, library: Library): ControlResult = {
    library.namedFunctionsMap.get(functionCall.name) match {
      case Some(namedFunction) =>
        controlParameters(namedFunction.parameters, functionCall.parameters, library, functionCall.location)
      case None =>
        unknownFunctionError(functionCall.name, functionCall.location)
    }
  }
}
