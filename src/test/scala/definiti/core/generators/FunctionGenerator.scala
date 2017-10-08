package definiti.core.generators

import definiti.core.ReferenceContext
import definiti.core.ast._
import definiti.core.ast.pure._
import org.scalacheck.Gen

object FunctionGenerator {
  def anyFunction(implicit context: ReferenceContext): Gen[PureDefinedFunction] = for {
    parameters <- Gen.listOf(anyParameterDefinition)
    body <- ExpressionGenerator.anyExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    location <- ASTGenerator.anyLocation
  } yield {
    PureDefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      location = location
    )
  }

  def anyFunctionWithParameters(numberOrParameters: Int)(implicit context: ReferenceContext): Gen[PureDefinedFunction] = for {
    parameters <- Gen.listOfN(numberOrParameters, anyParameterDefinition)
    body <- ExpressionGenerator.anyExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    location <- ASTGenerator.anyLocation
  } yield {
    PureDefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      location = location
    )
  }

  def anyFunctionReturningBoolean(implicit context: ReferenceContext): Gen[PureDefinedFunction] = for {
    parameters <- Gen.listOf(anyParameterDefinition)
    body <- ExpressionGenerator.anyBooleanExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    location <- ASTGenerator.anyLocation
  } yield {
    PureDefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      location = location
    )
  }

  def anyFunctionWithParametersReturningBoolean(numberOrParameters: Int)(implicit context: ReferenceContext): Gen[PureDefinedFunction] = for {
    parameters <- Gen.listOfN(numberOrParameters, anyParameterDefinition)
    body <- ExpressionGenerator.anyBooleanExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    location <- ASTGenerator.anyLocation
  } yield {
    PureDefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      location = location
    )
  }

  def anyReferencedFunction(implicit context: ReferenceContext): Gen[PureDefinedFunction] = for {
    parameters <- Gen.listOf(anyReferencedParameterDefinition)
    body <- ExpressionGenerator.anyReferencedExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    location <- ASTGenerator.anyLocation
  } yield {
    PureDefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      location = location
    )
  }

  def anyReferencedFunctionWithParameters(numberOrParameters: Int)(implicit context: ReferenceContext): Gen[PureDefinedFunction] = for {
    parameters <- Gen.listOfN(numberOrParameters, anyReferencedParameterDefinition)
    body <- ExpressionGenerator.anyReferencedExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    location <- ASTGenerator.anyLocation
  } yield {
    PureDefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      location = location
    )
  }

  def anyReferencedFunctionReturningBoolean(implicit context: ReferenceContext): Gen[PureDefinedFunction] = for {
    parameters <- Gen.listOf(anyReferencedParameterDefinition)
    body <- ExpressionGenerator.anyBooleanExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    location <- ASTGenerator.anyLocation
  } yield {
    PureDefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      location = location
    )
  }

  def anyReferencedFunctionWithParametersReturningBoolean(numberOrParameters: Int)(implicit context: ReferenceContext): Gen[PureDefinedFunction] = for {
    parameters <- Gen.listOfN(numberOrParameters, anyReferencedParameterDefinition)
    body <- ExpressionGenerator.anyBooleanExpression
    genericTypes <- ASTGenerator.listOfGenericTypeDefinition
    location <- ASTGenerator.anyLocation
  } yield {
    PureDefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      location = location
    )
  }

  def anyParameterDefinition(implicit context: ReferenceContext): Gen[ParameterDefinition] = for {
    name <- ASTGenerator.anyIdentifier
    typeReference <- ASTGenerator.anyTypeReference
    location <- ASTGenerator.anyLocation
  } yield {
    ParameterDefinition(
      name,
      typeReference,
      location
    )
  }

  def anyReferencedParameterDefinition(implicit context: ReferenceContext): Gen[ParameterDefinition] = for {
    name <- ASTGenerator.anyIdentifier
    typeReference <- ASTGenerator.referencedTypeReference
    location <- ASTGenerator.anyLocation
  } yield {
    ParameterDefinition(
      name,
      typeReference,
      location
    )
  }
}