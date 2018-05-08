package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.TopLevelParameterControlHelper

private[core] object VerificationParameterUsableControl extends Control[Root] with TopLevelParameterControlHelper {
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
