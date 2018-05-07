package definiti.core.validation.helpers

import definiti.common.ast._
import definiti.common.control.{Control, ControlResult}
import definiti.common.validation.Alert

private[core] trait TopLevelParameterControlHelper {
  self: Control =>

  private val authorizedTypes: Seq[TypeReference] = Seq("Boolean", "Number", "String").map(TypeReference(_))

  protected def controlParameter(parameter: ParameterInfo, library: Library): ControlResult = {
    parameter.typeReference match {
      case typeReference: TypeReference =>
        if (authorizedTypes.contains(typeReference)) {
          ControlResult.OK
        } else {
          errorUnauthorizedType(parameter.name, parameter.typeReference, parameter.location)
        }
      case _ => errorUnexpectedType(parameter.name, parameter.typeReference, parameter.location)
    }
  }

  def errorUnexpectedType(parameterName: String, typeReference: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"Unexpected type on parameter ${parameterName}: ${typeReference.readableString}",
      location
    )
  }

  def errorUnauthorizedType(parameterName: String, typeReference: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"Unauthorized type on parameter ${parameterName}: ${typeReference.readableString}, only atomic types are authorized (Boolean, Number, String)",
      location
    )
  }

  case class ParameterInfo(typeName: String, parameter: ParameterDefinition) {
    def name: String = s"${typeName}.${parameter.name}"
    def typeReference: AbstractTypeReference = parameter.typeReference
    def location: Location =  parameter.location
  }
}
