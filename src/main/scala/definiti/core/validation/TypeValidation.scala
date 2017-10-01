package definiti.core.validation

import definiti.core._
import definiti.core.ast.pure.AttributeDefinition
import definiti.core.ast.structure._
import definiti.core.ast.typed.TypeVerification

private[core] trait TypeValidation {
  self: ASTValidation =>

  protected def validateAliasType(aliasType: AliasType): Validation = {
    validateTypeReference(aliasType.alias, aliasType.range).verifyingAlso {
      Validation.join(aliasType.inherited.map(validateVerificationReference(_, aliasType.range)))
    }
  }

  protected def validateDefinedType(definedType: DefinedType): Validation = {
    val inheritedValidations = definedType.inherited.map(validateVerificationReference(_, definedType.range))
    val attributeValidations = definedType.attributes.map(validateAttributeDefinition)
    val verificationValidations = definedType.verifications.map(validateTypeVerification)
    Validation.join(inheritedValidations ++ attributeValidations ++ verificationValidations)
  }

  private def validateAttributeDefinition(attribute: AttributeDefinition): Validation = {
    val typeReferenceValidation = validateTypeReference(attribute.typeReference, attribute.range)
    val verificationsValidation = attribute.verifications.map(validateVerificationReference(_, attribute.range))
    Validation.join(typeReferenceValidation +: verificationsValidation)
  }

  private def validateTypeVerification(verification: TypeVerification): Validation = {
    validateDeepBooleanExpression(verification.function.body)
  }
}
