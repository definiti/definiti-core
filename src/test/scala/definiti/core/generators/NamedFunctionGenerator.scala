package definiti.core.generators

import definiti.core._
import definiti.core.ast.pure._
import org.scalacheck.Gen

object NamedFunctionGenerator {
  def anyNamedFunctionWithoutPackage(implicit context: ReferenceContext): Gen[PureNamedFunction] = for {
    name <- ASTGenerator.anyIdentifier
    parameters <- Gen.listOf(FunctionGenerator.anyParameterDefinition)
    body <- ExpressionGenerator.anyExpression
    genericTypes <- Gen.listOf(ASTGenerator.anyIdentifier)
    returnType <- ASTGenerator.anyTypeReference
    location <- ASTGenerator.anyLocation
  } yield {
    PureNamedFunction(
      name = name,
      packageName = NOT_DEFINED,
      genericTypes = genericTypes,
      parameters = parameters,
      returnType = returnType,
      body: PureExpression,
      location = location
    )
  }
}
