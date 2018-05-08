package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.TypeReferenceControlHelper

private[core] object NamedFunctionTypeControl extends Control[Root] with TypeReferenceControlHelper {
  override val description: String = "Check if the type of the body of the named function match the defined one"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.namedFunctions.map(controlNamedFunction(_, library))
    }
  }

  private def controlNamedFunction(namedFunction: NamedFunction, library: Library): ControlResult = {
    controlReturnTypeReference(namedFunction, library) + controlReturnTypeEquality(namedFunction)
  }

  private def controlReturnTypeReference(namedFunction: NamedFunction, library: Library): ControlResult = {
    controlTypeReference(
      typeReference = namedFunction.returnType,
      availableGenerics = namedFunction.genericTypes,
      location = namedFunction.location,
      library = library
    )
  }

  private def controlReturnTypeEquality(namedFunction: NamedFunction): ControlResult = {
    val declaredType = namedFunction.returnType.readableString
    val realType = namedFunction.body.returnType.readableString
    if (declaredType == realType) {
      OK
    } else {
      ControlResult(errorDifferentType(namedFunction.fullName, declaredType, realType, namedFunction.location))
    }
  }

  def errorDifferentType(functionName: String, declaredType: String, realType: String, location: Location): Alert = {
    alert(
      s"The declared type of the function ${functionName} and its body are different (${realType} instead of ${declaredType})",
      location
    )
  }
}
