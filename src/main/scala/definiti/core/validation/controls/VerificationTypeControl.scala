package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.TypeReferenceControlHelper

object VerificationTypeControl extends Control with TypeReferenceControlHelper {
  override val description: String = "Check if the type of the verification is valid"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.verifications.map(controlVerification(_, library))
    }
  }

  private def controlVerification(verification: Verification, library: Library): ControlResult = {
    verification.function.parameters.headOption match {
      case Some(parameter) => controlAbstractTypeReference(parameter.typeReference, verification, parameter.location, library)
      case None => ControlResult(errorNoParameter(verification))
    }
  }

  private def controlAbstractTypeReference(typeReference: AbstractTypeReference, verification: Verification, location: Location, library: Library): ControlResult = {
    typeReference match {
      case typeReference: TypeReference => controlTypeReference(typeReference, verification, location, library)
      case _ => ControlResult(errorUnexpectedType(verification.fullName, typeReference, location))
    }
  }

  private def controlTypeReference(typeReference: TypeReference, verification: Verification, location: Location, library: Library): ControlResult = {
    controlTypeReference(
      typeReference = typeReference,
      elementName = verification.fullName,
      availableGenerics = verification.function.genericTypes,
      location = location,
      library = library
    )
  }

  def errorNoParameter(verification: Verification): Alert = {
    alert(s"Verification ${verification.fullName} does not have any parameter", verification.function.location)
  }

  def errorUnexpectedType(verificationName: String, typeReference: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"Unexpected type on verification ${verificationName}: ${typeReference.readableString}. Verifications must target classes.",
      location
    )
  }
}
