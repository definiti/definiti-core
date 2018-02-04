package definiti.core.validation.controls

import definiti.core.ast._
import definiti.core.validation.{Control, ControlLevel, ControlResult}
import definiti.core.{Alert, BOOLEAN}

object VerificationIsBooleanControl extends Control {
  override val description: String = "Checks if the expression inside verification is Boolan"
  override val defaultLevel: ControlLevel.Value = ControlLevel.error

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
    alert(
      s"The expression inside verification ${typeName} is not a boolean expression, but a ${returnType.readableString}",
      location
    )
  }
}
