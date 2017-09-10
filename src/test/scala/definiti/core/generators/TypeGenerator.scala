package definiti.core.generators

import definiti.core._
import definiti.core.ast.pure._
import org.scalacheck.Gen

object TypeGenerator {
  def anyType(implicit context: ReferenceContext): Gen[Type] = Gen.oneOf(anyDefinedType, anyAliasType)

  def anyDefinedType(implicit context: ReferenceContext): Gen[DefinedType] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    genericTypes <- ASTGenerator.listOfGenericTypeDefinition
    attributes <- listOfAttributes
    verifications <- listOfTypeVerifications
    inherited <- Gen.listOf(VerificationGenerator.anyVerificationReference)
    comment <- Gen.option(ASTGenerator.anyString)
    range <- ASTGenerator.anyRange
  } yield {
    DefinedType(
      name,
      packageName,
      genericTypes,
      attributes,
      verifications,
      inherited,
      comment,
      range
    )
  }

  def anyAliasType(implicit context: ReferenceContext): Gen[AliasType] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    genericTypes <- ASTGenerator.listOfGenericTypeDefinition
    alias <- ASTGenerator.referencedTypeReference
    inherited <- Gen.listOf(VerificationGenerator.anyVerificationReference)
    comment <- Gen.option(ASTGenerator.anyString)
    range <- ASTGenerator.anyRange
  } yield {
    AliasType(
      name,
      packageName,
      genericTypes,
      alias,
      inherited,
      comment,
      range
    )
  }

  def anyAttributeDefinition(implicit context: Context): Gen[AttributeDefinition] = for {
    name <- ASTGenerator.anyIdentifier
    typeReference <- ASTGenerator.anyTypeReference
    comment <- Gen.option(ASTGenerator.anyString)
    verifications <- Gen.listOf(VerificationGenerator.anyVerificationReference)
    range <- ASTGenerator.anyRange
  } yield {
    AttributeDefinition(
      name = name,
      typeReference = typeReference,
      comment = comment,
      verifications = verifications,
      range = range
    )
  }

  def anyTypeVerification(implicit context: ReferenceContext): Gen[TypeVerification] = for {
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyFunctionWithParametersReturningBoolean(1)
    range <- ASTGenerator.anyRange
  } yield {
    TypeVerification(
      message,
      function,
      range
    )
  }

  def listOfAttributes(implicit context: ReferenceContext): Gen[List[AttributeDefinition]] = {
    Generators.listOfBoundedSize(0, 5, anyAttributeDefinition)
  }

  def listOfTypeVerifications(implicit context: ReferenceContext): Gen[List[TypeVerification]] = {
    Generators.listOfBoundedSize(0, 3, anyTypeVerification)
  }
}