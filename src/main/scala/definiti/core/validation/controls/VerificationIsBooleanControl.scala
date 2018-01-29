package definiti.core.validation.controls

import definiti.core.{Alert, AlertControl, BOOLEAN}
import definiti.core.ast._

object VerificationIsBooleanControl extends Control {
  override def name: String = "verificationIsBoolean"

  override def description: String = "Checks if the expression inside verification is Boolan"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.verifications.map(controlVerification)
    }
  }

  private def controlVerification(verification: Verification): ControlResult = {
    if (verification.function.body.returnType == TypeReference(BOOLEAN)) {
      OK
    } else {
      ControlResult(errorNotBoolean(verification.fullName, verification.function.body.returnType, verification.function.location))
    }
  }

  def errorNotBoolean(typeName: String, returnType: AbstractTypeReference, location: Location): Alert = {
    AlertControl(
      name,
      s"The expression inside verification ${typeName} is not a boolean expression, but a ${returnType.readableString}",
      location
    )
  }
}
