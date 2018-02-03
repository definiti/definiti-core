package definiti.core.validation.controls

import definiti.core._
import definiti.core.ast._

object TypeVerificationIsBooleanControl extends Control {

  override def name: String = "typeVerificationIsBoolean"

  override def description: String = "Checks if the expression inside type verification is Boolean"

  override def defaultLevel: ControlLevel.Value = ControlLevel.error

  override def control(root: Root, library: Library): ControlResult = {
    ControlResult.squash {
      val typeVerifications = extractTypeVerificationInfo(library)
      typeVerifications.map(controlTypeVerification)
    }
  }

  private def extractTypeVerificationInfo(library: Library): Seq[TypeVerificationInfo] = {
    val aliasTypeVerifications = library.aliasTypes.flatMap { aliasType =>
      aliasType.verifications.map { verification =>
        TypeVerificationInfo(aliasType.fullName, verification)
      }
    }
    val definedTypeVerifications = library.definedTypes.flatMap { aliasType =>
      aliasType.verifications.map { verification =>
        TypeVerificationInfo(aliasType.fullName, verification)
      }
    }
    aliasTypeVerifications ++ definedTypeVerifications
  }

  private def controlTypeVerification(typeVerificationInfo: TypeVerificationInfo): ControlResult = {
    if (typeVerificationInfo.verification.function.body.returnType == TypeReference("Boolean")) {
      OK
    } else {
      ControlResult(errorNotBoolean(
        typeName = typeVerificationInfo.typeName,
        message = typeVerificationInfo.verification.message,
        returnType = typeVerificationInfo.verification.function.body.returnType,
        location = typeVerificationInfo.verification.function.location
      ))
    }
  }

  def errorNotBoolean(typeName: String, message: String, returnType: AbstractTypeReference, location: Location): Alert = {
    AlertControl(
      name,
      s"""The expression inside type verification ${typeName}("${message}") is not a boolean expression, but a ${returnType.readableString}""",
      location
    )
  }

  private case class TypeVerificationInfo(typeName: String, verification: TypeVerification)

}
