package definiti.core.generators

import definiti.core.generators.ASTGenerator.anyLocation
import definiti.core._
import definiti.core.ast.pure._
import org.scalacheck.Gen

object ExpressionGenerator {
  // TODO: make it complete when needed
  def anyExpression(implicit context: Context): Gen[PureExpression] = for {
    location <- anyLocation
  } yield {
    PureBooleanValue(value = true, location = location)
  }

  // TODO: make it complete when needed
  def anyBooleanExpression(implicit context: Context): Gen[PureExpression] = for {
    location <- anyLocation
  } yield {
    PureBooleanValue(value = true, location = location)
  }

  // TODO: make it complete when needed
  def anyReferencedExpression(implicit context: Context): Gen[PureExpression] = for {
    location <- anyLocation
  } yield {
    PureBooleanValue(value = true, location = location)
  }

  def anyFunctionCall(implicit context: Context): Gen[PureExpression] = anyFunctionCall(5)

  def anyFunctionCall(limit: Int)(implicit context: Context): Gen[PureExpression] = for {
    name <- Generators.anyIdentifier
    parameters <- Gen.listOf(anyExpression)
    generics <- Gen.listOf(ASTGenerator.anyTypeReference)
    location <- ASTGenerator.anyLocation
  } yield {
    PureFunctionCall(
      name = name,
      parameters = parameters,
      generics = generics,
      location = location
    )
  }
}
