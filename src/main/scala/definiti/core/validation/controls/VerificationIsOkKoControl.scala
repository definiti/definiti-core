package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers._

private[core] object VerificationIsOkKoControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper with OkKoControlHelper {
  override def description: String = "Check if the verification ends with a OkKo type in case of typed messages and its types are valid"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.verifications
        .filter(_.message.isInstanceOf[TypedMessage])
        .map(controlVerification(_, library))
    }
  }

  private def controlVerification(verification: Verification, library: Library): ControlResult = {
    if (verification.function.body.returnType == TypeReference("OkKo")) {
      controlKoTypes(verification, library)
    } else {
      ControlResult(errorNotOkKo(verification.fullName, verification.function.body.returnType, verification.function.location))
    }
  }

  private def controlKoTypes(verification: Verification, library: Library): ControlResult = {
    deepControl(verification.function.body) {
      case koValue: KoValue =>
        controlTypes(
          expectedTypes = verification.message.asInstanceOf[TypedMessage].types,
          gotExpressions = koValue.parameters,
          location = koValue.location,
          library = library
        )
    }
  }

  def errorNotOkKo(typeName: String, returnType: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"The expression inside verification ${typeName} is not a OkKo expression, but a ${returnType.readableString}",
      location
    )
  }
}
