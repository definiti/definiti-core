package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers.{ParameterControlHelper, TypeReferenceControlHelper}

private[core] object VerificationReferenceParametersControl extends Control with ParameterControlHelper with TypeReferenceControlHelper {
  override val description: String = "Check if a type or attribute references a verification with valid types"

  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      extractVerificationReferences(library)
        .map(controlVerificationReference(_, library))
    }
  }

  private def extractVerificationReferences(library: Library): Seq[VerificationReference] = {
    val aliasInherited = library.aliasTypes.flatMap(_.inherited)
    val definedInherited = library.definedTypes.flatMap(_.inherited)
    val definedAttributes = library.definedTypes.flatMap(_.attributes).flatMap(_.verifications)

    aliasInherited ++ definedInherited ++ definedAttributes
  }

  private def controlVerificationReference(verificationReference: VerificationReference, library: Library): ControlResult = {
    library.verificationsMap.get(verificationReference.verificationName) match {
      case Some(verification) =>
        val expectedParameters = verification.parameters
        val expectedNumberOfParameters = expectedParameters.length
        val gotParameters = verificationReference.parameters
        val gotNumberOfParameters = gotParameters.length

        if (gotNumberOfParameters == expectedNumberOfParameters) {
          controlParameters(expectedParameters, gotParameters, library, verificationReference.location)
        } else if (gotNumberOfParameters == expectedNumberOfParameters + 1) {
          if (gotParameters.last.isInstanceOf[QuotedStringValue]) {
            controlParameters(expectedParameters, gotParameters.take(expectedNumberOfParameters), library, verificationReference.location)
          } else {
            invalidNumberOfParameters(expectedNumberOfParameters, gotNumberOfParameters, verificationReference.location)
          }
        } else {
          invalidNumberOfParameters(expectedNumberOfParameters, gotNumberOfParameters, verificationReference.location)
        }
      case None =>
        unknownVerification(verificationReference.verificationName, verificationReference.location)
    }
  }

  def unknownVerification(verification: String, location: Location): Alert = {
    alert(
      s"Unknown verification ${verification}",
      location
    )
  }
}
