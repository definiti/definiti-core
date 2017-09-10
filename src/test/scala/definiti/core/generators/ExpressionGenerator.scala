package definiti.core.generators

import definiti.core.generators.ASTGenerator.anyRange
import definiti.core._
import definiti.core.ast.pure._
import org.scalacheck.Gen

object ExpressionGenerator {
  // TODO: make it complete when needed
  def anyExpression(implicit context: Context): Gen[Expression] = for {
    range <- anyRange
  } yield {
    BooleanValue(value = true, range = range)
  }

  // TODO: make it complete when needed
  def anyBooleanExpression(implicit context: Context): Gen[Expression] = for {
    range <- anyRange
  } yield {
    BooleanValue(value = true, range = range)
  }

  // TODO: make it complete when needed
  def anyReferencedExpression(implicit context: Context): Gen[Expression] = for {
    range <- anyRange
  } yield {
    BooleanValue(value = true, range = range)
  }

  def anyFunctionCall(implicit context: Context): Gen[Expression] = anyFunctionCall(5)

  def anyFunctionCall(limit: Int)(implicit context: Context): Gen[Expression] = for {
    name <- Generators.anyIdentifier
    parameters <- Gen.listOf(anyExpression)
    generics <- Gen.listOf(ASTGenerator.anyTypeReference)
    range <- ASTGenerator.anyRange
  } yield {
    FunctionCall(
      name = name,
      parameters = parameters,
      generics = generics,
      range = range
    )
  }
}
