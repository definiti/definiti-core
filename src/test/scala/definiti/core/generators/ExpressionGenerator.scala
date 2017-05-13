package definiti.core.generators

import definiti.core.{BooleanValue, Context, Expression}
import definiti.core.generators.ASTGenerator.anyRange
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
}
