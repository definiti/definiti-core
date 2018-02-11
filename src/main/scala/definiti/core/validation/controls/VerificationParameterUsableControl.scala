package definiti.core.validation.controls

import definiti.core.ast._
import definiti.core.validation._
import definiti.core.validation.helpers.TopLevelParameterControlHelper

object VerificationParameterUsableControl extends Control with TopLevelParameterControlHelper {
  override val description: String = "Check if parameter types in verification are valid"

  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      extractParameters(library)
        .map(controlParameter(_, library))
    }
  }

  private def extractParameters(library: Library): Seq[ParameterInfo] = {
    library.verifications.flatMap { verification =>
      verification.parameters.map(ParameterInfo(verification.fullName, _))
    }
  }
}
