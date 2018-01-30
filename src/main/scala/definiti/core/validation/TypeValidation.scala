package definiti.core.validation

import definiti.core._
import definiti.core.ast._

private[core] trait TypeValidation {
  self: ASTValidation =>

  protected def validateAliasType(aliasType: AliasType): Validation = {
    val aliasValidation = validateTypeReference(aliasType.alias, aliasType.genericTypes, aliasType.location)
    val verificationValidations = aliasType.verifications.map(validateTypeVerification)
    Validation.join(aliasValidation +: verificationValidations)
  }

  protected def validateDefinedType(definedType: DefinedType): Validation = {
    val attributeValidations = definedType.attributes.map(validateAttributeDefinition(_, definedType.genericTypes))
    val verificationValidations = definedType.verifications.map(validateTypeVerification)
    Validation.join(attributeValidations ++ verificationValidations)
  }

  private def validateAttributeDefinition(attribute: AttributeDefinition, genericTypes: Seq[String]): Validation = {
    validateTypeReference(attribute.typeReference, genericTypes, attribute.location)
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
