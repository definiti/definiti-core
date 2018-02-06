package definiti.core.validation.controls

import definiti.core.Alert
import definiti.core.ast._
import definiti.core.validation._
import definiti.core.validation.helpers._

object VerificationIsOkKoControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper {
  override def description: String = "Check if the verification ends with a OkKo type in case of typed messages and its types are valid"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      library.verifications
        .filter(_.message.isInstanceOf[TypedMessage])
        .map(controlVerification)
    }
  }

  private def controlVerification(verification: Verification): ControlResult = {
    if (verification.function.body.returnType == TypeReference("OkKo")) {
      controlKoTypes(verification)
    } else {
      ControlResult(errorNotOkKo(verification.fullName, verification.function.body.returnType, verification.function.location))
    }
  }

  private def controlKoTypes(verification: Verification): ControlResult = {
    deepControl(verification.function.body) {
      case koValue: KoValue =>
        controlTypes(
          expectedTypes = verification.message.asInstanceOf[TypedMessage].types,
          gotExpressions = koValue.parameters,
          location = koValue.location
        )
    }
  }

  private def controlTypes(expectedTypes: Seq[TypeReference], gotExpressions: Seq[Expression], location: Location): ControlResult = {
    if (expectedTypes.length == gotExpressions.length) {
      ControlResult.squash {
        expectedTypes.zip(gotExpressions)
          .map { case (expectedParameter, gotExpression) => controlType(expectedParameter, gotExpression.returnType, gotExpression.location) }
      }
    } else {
      invalidNumberOfParameters(expectedTypes.length, gotExpressions.length, location)
    }
  }

  private def controlType(expectedType: TypeReference, gotType: AbstractTypeReference, location: Location): ControlResult = {
    gotType match {
      case gotTypeReference: TypeReference if areTypeEqual(expectedType, gotTypeReference) => OK
      case _ => invalidType(expectedType, gotType, location)
    }
  }

  def errorNotOkKo(typeName: String, returnType: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"The expression inside verification ${typeName} is not a OkKo expression, but a ${returnType.readableString}",
      location
    )
  }

  def invalidNumberOfParameters(expected: Int, got: Int, location: Location): Alert = {
    alert(
      s"Expected ${expected} parameters, got ${got}",
      location
    )
  }

  def invalidType(expected: TypeReference, got: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"Expected type ${expected.readableString}, got ${got.readableString}",
      location
    )
  }
}
