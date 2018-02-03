package definiti.core.validation

import definiti.core._
import definiti.core.ast._

private[core] trait TypeValidation {
  self: ASTValidation =>

  protected def validateAliasType(aliasType: AliasType): Validation = {
    Validation.join(aliasType.verifications.map(validateTypeVerification))
  }

  protected def validateDefinedType(definedType: DefinedType): Validation = {
    Validation.join(definedType.verifications.map(validateTypeVerification))
  }

  private def validateTypeVerification(verification: TypeVerification): Validation = {
    validateDeepBooleanExpression(verification.function.body)
  }

  protected def validateEnum(enum: Enum): Validation = {
    Validation.join {
      enum.cases.zipWithIndex.map { case (enumCase, index) =>
        if (enum.cases.indexWhere(_.name == enumCase.name) == index) {
          Valid
        } else {
          Invalid(s"The case ${enumCase.name} is already defined in enum ${enum.name}", enumCase.location)
        }
      }
    }
  }
}
