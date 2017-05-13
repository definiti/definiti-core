package definiti.core.generators

import definiti.core._
import org.scalacheck.Gen

object VerificationGenerator {
  def anyVerification(implicit context: ReferenceContext): Gen[Verification] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyFunction
    comment <- Gen.option(ASTGenerator.anyString)
    range <- ASTGenerator.anyRange
  } yield {
    Verification(
      name = name,
      packageName = packageName,
      message = message,
      function = function,
      comment = comment,
      range = range
    )
  }

  def anyReferencedVerification(implicit context: ReferenceContext): Gen[Verification] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyReferencedFunction
    comment <- Gen.option(ASTGenerator.anyString)
    range <- ASTGenerator.anyRange
  } yield {
    Verification(
      name = name,
      packageName = packageName,
      message = message,
      function = function,
      comment = comment,
      range = range
    )
  }

  def anyValidVerification(implicit context: ReferenceContext): Gen[Verification] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyFunctionWithParametersReturningBoolean(1)
    comment <- Gen.option(ASTGenerator.anyString)
    range <- ASTGenerator.anyRange
  } yield {
    Verification(
      name = name,
      packageName = packageName,
      message = message,
      function = function,
      comment = comment,
      range = range
    )
  }

  def anyReferencedValidVerification(implicit context: ReferenceContext): Gen[Verification] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyReferencedFunctionWithParametersReturningBoolean(1)
    comment <- Gen.option(ASTGenerator.anyString)
    range <- ASTGenerator.anyRange
  } yield {
    Verification(
      name = name,
      packageName = packageName,
      message = message,
      function = function,
      comment = comment,
      range = range
    )
  }
}
