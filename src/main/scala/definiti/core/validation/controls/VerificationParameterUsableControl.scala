package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation._

object VerificationParameterUsableControl extends Control {
  override val description: String = "Check if parameter types in verification are valid"

  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  private val authorizedTypes: Seq[TypeReference] = Seq("Boolean", "Number", "String").map(TypeReference(_))

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.verifications.map(controlVerification(_, library))
    }
  }

  private def controlVerification(verification: Verification, library: Library): ControlResult = {
    ControlResult.squash {
      verification.parameters.map(controlParameter(_, verification, library))
    }
  }

  private def controlParameter(parameter: ParameterDefinition, verification: Verification, library: Library): ControlResult = {
    val parameterName = s"${verification.fullName}.${parameter.name}"
    parameter.typeReference match {
      case typeReference: TypeReference =>
        if (authorizedTypes.contains(typeReference)) {
          ControlResult.OK
        } else {
          errorUnauthorizedType(parameterName, parameter.typeReference, parameter.location)
        }
      case _ => errorUnexpectedType(parameterName, parameter.typeReference, parameter.location)
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
}
