package definiti.core.validation

import definiti.core._
import definiti.core.ast._

private[core] trait TypeValidation {
  self: ASTValidation =>

  protected def validateAliasType(aliasType: AliasType): Validation = {
    validateTypeReference(aliasType.alias, aliasType.genericTypes, aliasType.location).verifyingAlso {
      Validation.join(aliasType.inherited.map(validateVerificationReference(_, aliasType.location)))
    }
  }

  protected def validateDefinedType(definedType: DefinedType): Validation = {
    val inheritedValidations = definedType.inherited.map(validateVerificationReference(_, definedType.location))
    val attributeValidations = definedType.attributes.map(validateAttributeDefinition(_, definedType.genericTypes))
    val verificationValidations = definedType.verifications.map(validateTypeVerification)
    Validation.join(inheritedValidations ++ attributeValidations ++ verificationValidations)
  }

  private def validateAttributeDefinition(attribute: AttributeDefinition, genericTypes: Seq[String]): Validation = {
    val typeReferenceValidation = validateTypeReference(attribute.typeReference, genericTypes, attribute.location)
    val verificationsValidation = attribute.verifications.map(validateVerificationReference(_, attribute.location))
    Validation.join(typeReferenceValidation +: verificationsValidation)
  }

  private def validateTypeVerification(verification: TypeVerification): Validation = {
    validateDeepBooleanExpression(verification.function.body)
  }
}
