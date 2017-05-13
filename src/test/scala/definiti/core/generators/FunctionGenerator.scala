package definiti.core.generators

import definiti.core.{DefinedFunction, ParameterDefinition, ReferenceContext}
import org.scalacheck.Gen

object FunctionGenerator {
  def anyFunction(implicit context: ReferenceContext): Gen[DefinedFunction] = for {
    parameters <- Gen.listOf(anyParameterDefinition)
    body <- ExpressionGenerator.anyExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    range <- ASTGenerator.anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  def anyFunctionWithParameters(numberOrParameters: Int)(implicit context: ReferenceContext): Gen[DefinedFunction] = for {
    parameters <- Gen.listOfN(numberOrParameters, anyParameterDefinition)
    body <- ExpressionGenerator.anyExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    range <- ASTGenerator.anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  def anyFunctionReturningBoolean(implicit context: ReferenceContext): Gen[DefinedFunction] = for {
    parameters <- Gen.listOf(anyParameterDefinition)
    body <- ExpressionGenerator.anyBooleanExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    range <- ASTGenerator.anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  def anyFunctionWithParametersReturningBoolean(numberOrParameters: Int)(implicit context: ReferenceContext): Gen[DefinedFunction] = for {
    parameters <- Gen.listOfN(numberOrParameters, anyParameterDefinition)
    body <- ExpressionGenerator.anyBooleanExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    range <- ASTGenerator.anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  def anyReferencedFunction(implicit context: ReferenceContext): Gen[DefinedFunction] = for {
    parameters <- Gen.listOf(anyReferencedParameterDefinition)
    body <- ExpressionGenerator.anyReferencedExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    range <- ASTGenerator.anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  def anyReferencedFunctionWithParameters(numberOrParameters: Int)(implicit context: ReferenceContext): Gen[DefinedFunction] = for {
    parameters <- Gen.listOfN(numberOrParameters, anyReferencedParameterDefinition)
    body <- ExpressionGenerator.anyReferencedExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    range <- ASTGenerator.anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  def anyReferencedFunctionReturningBoolean(implicit context: ReferenceContext): Gen[DefinedFunction] = for {
    parameters <- Gen.listOf(anyReferencedParameterDefinition)
    body <- ExpressionGenerator.anyBooleanExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    range <- ASTGenerator.anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  def anyReferencedFunctionWithParametersReturningBoolean(numberOrParameters: Int)(implicit context: ReferenceContext): Gen[DefinedFunction] = for {
    parameters <- Gen.listOfN(numberOrParameters, anyReferencedParameterDefinition)
    body <- ExpressionGenerator.anyBooleanExpression
    genericTypes <- ASTGenerator.listOfGenericTypeDefinition
    range <- ASTGenerator.anyRange
  } yield {
    DefinedFunction(
      parameters = parameters,
      body = body,
      genericTypes = genericTypes,
      range = range
    )
  }

  def anyParameterDefinition(implicit context: ReferenceContext): Gen[ParameterDefinition] = for {
    name <- ASTGenerator.anyIdentifier
    typeReference <- ASTGenerator.anyTypeReference
    range <- ASTGenerator.anyRange
  } yield {
    ParameterDefinition(
      name,
      typeReference,
      range
    )
  }

  def anyReferencedParameterDefinition(implicit context: ReferenceContext): Gen[ParameterDefinition] = for {
    name <- ASTGenerator.anyIdentifier
    typeReference <- ASTGenerator.referencedTypeReference
    range <- ASTGenerator.anyRange
  } yield {
    ParameterDefinition(
      name,
      typeReference,
      range
    )
  }
}