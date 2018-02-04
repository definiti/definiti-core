package definiti.core.validation.controls

import definiti.core.ast._
import definiti.core.validation.controls.helpers.{ExpressionControlHelper, ParameterControlHelper, TypeReferenceControlHelper}

object FunctionParametersControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper with ParameterControlHelper {
  override def name: String = "functionParameters"

  override def description: String = "Check if parameters and arguments on a named function are the same"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

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
