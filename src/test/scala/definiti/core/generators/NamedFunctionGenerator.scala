package definiti.core.generators

import definiti.core._
import definiti.core.ast.pure._
import org.scalacheck.Gen

object NamedFunctionGenerator {
  def anyNamedFunctionWithoutPackage(implicit context: ReferenceContext): Gen[NamedFunction] = for {
    name <- ASTGenerator.anyIdentifier
    parameters <- Gen.listOf(FunctionGenerator.anyParameterDefinition)
    body <- ExpressionGenerator.anyExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    returnType <- ASTGenerator.anyTypeReference
    range <- ASTGenerator.anyRange
  } yield {
    NamedFunction(
      name = name,
      packageName = NOT_DEFINED,
      genericTypes = genericTypes,
      parameters = parameters,
      returnType = returnType,
      body: Expression,
      range = range
    )
  }
}
