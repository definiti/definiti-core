package definiti.core.validation

import definiti.core.Validation
import definiti.core.ast.NamedFunction

private[core] trait NamedFunctionValidation {
  self: ASTValidation =>

  protected def validateNamedFunction(namedFunction: NamedFunction): Validation = {
    validateTypeReference(namedFunction.returnType, namedFunction.location)
  }
}
