package definiti.core.validation.controls

import definiti.common.ast._
import definiti.common.control.{Control, ControlLevel, ControlResult}
import definiti.common.validation.Alert
import definiti.core.validation.helpers._

private[core] object TypeVerificationIsOkKoControl extends Control with ExpressionControlHelper with TypeReferenceControlHelper with OkKoControlHelper {
  override def description: String = "Check if the type verification ends with a Okko type in case of typed messages and its types are valid"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      val typeVerifications = extractTypeVerificationInfo(library)
      typeVerifications.map(controlTypeVerification(_, library))
    }
  }

  private def extractTypeVerificationInfo(library: Library): Seq[TypeVerificationInfo] = {
    val aliasTypeVerifications = library.aliasTypes.flatMap { aliasType =>
      aliasType.verifications.collect {
        case verification if verification.message.isInstanceOf[TypedMessage] =>
          TypeVerificationInfo(aliasType.fullName, verification)
      }
    }
    val definedTypeVerifications = library.definedTypes.flatMap { aliasType =>
      aliasType.verifications.collect {
        case verification if verification.message.isInstanceOf[TypedMessage] =>
          TypeVerificationInfo(aliasType.fullName, verification)
      }
    }
    aliasTypeVerifications ++ definedTypeVerifications
  }

  private def controlTypeVerification(typeVerificationInfo: TypeVerificationInfo, library: Library): ControlResult = {
    if (typeVerificationInfo.verification.function.body.returnType == TypeReference("OkKo")) {
      controlKoTypes(typeVerificationInfo.verification, library)
    } else {
      ControlResult(errorNotOkKo(
        typeName = typeVerificationInfo.typeName,
        message = typeVerificationInfo.verification.message,
        returnType = typeVerificationInfo.verification.function.body.returnType,
        location = typeVerificationInfo.verification.function.location
      ))
    }
  }

  private def controlKoTypes(verification: TypeVerification, library: Library): ControlResult = {
    deepControl(verification.function.body) {
      case koValue: KoValue =>
        controlTypes(
          expectedTypes = verification.message.asInstanceOf[TypedMessage].types,
          gotExpressions = koValue.parameters,
          location = koValue.location,
          library
        )
    }
  }

  def errorNotOkKo(typeName: String, message: VerificationMessage, returnType: AbstractTypeReference, location: Location): Alert = {
    alert(
      s"""The expression inside type verification ${typeName}(${message.prettyPrint}) is not a OkKo expression, but a ${returnType.readableString}""",
      location
    )
  }

  private case class TypeVerificationInfo(typeName: String, verification: TypeVerification)

}
