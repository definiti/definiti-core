package definiti.core.generators

import definiti.core._
import definiti.core.ast._
import definiti.core.ast.pure._
import org.scalacheck.Gen

object VerificationGenerator {
  def anyVerification(implicit context: ReferenceContext): Gen[PureVerification] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyFunction
    comment <- Gen.option(ASTGenerator.anyString)
    location <- ASTGenerator.anyLocation
  } yield {
    PureVerification(
      name = name,
      packageName = packageName,
      message = message,
      function = function,
      comment = comment,
      location = location
    )
  }

  def anyReferencedVerification(implicit context: ReferenceContext): Gen[PureVerification] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyReferencedFunction
    comment <- Gen.option(ASTGenerator.anyString)
    location <- ASTGenerator.anyLocation
  } yield {
    PureVerification(
      name = name,
      packageName = packageName,
      message = message,
      function = function,
      comment = comment,
      location = location
    )
  }

  def anyValidVerification(implicit context: ReferenceContext): Gen[PureVerification] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyFunctionWithParametersReturningBoolean(1)
    comment <- Gen.option(ASTGenerator.anyString)
    location <- ASTGenerator.anyLocation
  } yield {
    PureVerification(
      name = name,
      packageName = packageName,
      message = message,
      function = function,
      comment = comment,
      location = location
    )
  }

  def anyReferencedValidVerification(implicit context: ReferenceContext): Gen[PureVerification] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    message <- ASTGenerator.anyString
    function <- FunctionGenerator.anyReferencedFunctionWithParametersReturningBoolean(1)
    comment <- Gen.option(ASTGenerator.anyString)
    location <- ASTGenerator.anyLocation
  } yield {
    PureVerification(
      name = name,
      packageName = packageName,
      message = message,
      function = function,
      comment = comment,
      location = location
    )
  }

  def anyVerificationReference(implicit context: Context): Gen[VerificationReference] = for {
    verificationName <- ASTGenerator.anyIdentifier
    message <- Gen.option(ASTGenerator.anyString)
    location <- ASTGenerator.anyLocation
  } yield {
    VerificationReference(
      verificationName = verificationName,
      message = message,
      location = location
    )
  }

  def anyReferencedVerificationReference(implicit context: ReferenceContext): Gen[VerificationReference] = for {
    verificationName <- Gen.oneOf(context.verifications).map(_.canonicalName)
    message <- Gen.option(ASTGenerator.anyString)
    location <- ASTGenerator.anyLocation
  } yield {
    VerificationReference(
      verificationName = verificationName,
      message = message,
      location = location
    )
  }
}
