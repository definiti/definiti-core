package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert

private[core] object VerificationIsBooleanControl extends Control {
  override val description: String = "Checks if the expression inside verification is Boolean is case of literal messages"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.verifications
        .filter(_.message.isInstanceOf[LiteralMessage])
        .map(controlVerification)
    }
  }

  private def controlVerification(verification: Verification): ControlResult = {
    if (verification.function.body.returnType == TypeReference("Boolean")) {
      OK
    } else {
      ControlResult(errorNotBoolean(verification.fullName, verification.function.body.returnType, verification.function.location))
    }
  }

  def errorNotBoolean(typeName: String, returnType: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"The expression inside verification ${typeName} is not a boolean expression, but a ${returnType.readableString}",
      location
    )
  }
}
