package definiti.core.generators

import definiti.core.parser._
import definiti.core.{NamedFunction, ReferenceContext}
import org.scalacheck.Gen

object NamedFunctionGenerator {
  def anyNamedFunctionWithoutPackage(implicit context: ReferenceContext): Gen[NamedFunction] = for {
    name <- ASTGenerator.anyIdentifier
    function <- FunctionGenerator.anyFunction
    range <- ASTGenerator.anyRange
  } yield {
    NamedFunction(
      name = name,
      packageName = NOT_DEFINED,
      function = function,
      range = range
    )
  }
}
