package definiti.core.validation.controls.naming

import definiti.common.ast.{Library, Root, Verification}
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.core.validation.helpers.NameFormatHelper

object VerificationUpperCamelCaseControl extends Control[Root] with NameFormatHelper {
  override def description: String = "Verification names must be in UpperCamelCase format"

  override def defaultLevel: ControlLevel.Value = ControlLevel.warning

  override def control(value: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.verifications.map(controlVerification)
    }
  }

  private def controlVerification(verification: Verification): ControlResult = {
    controlUpperCamelCaseFormat(verification.name, verification.location)
  }
}
