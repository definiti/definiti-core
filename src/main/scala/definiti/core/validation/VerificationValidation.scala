package definiti.core.validation

import definiti.core.ast.Range
import definiti.core.ast.pure.VerificationReference
import definiti.core.ast.structure.Verification
import definiti.core.{Invalid, Valid, Validation}

private[core] trait VerificationValidation {
  self: ASTValidation =>

  protected def validateVerification(verification: Verification): Validation = {
    validateDeepBooleanExpression(verification.function.body)
  }

  protected def validateVerificationReference(verification: VerificationReference, range: Range): Validation = {
    if (library.verifications.contains(verification.verificationName)) {
      Valid
    } else {
      Invalid("Undefined verification: " + verification.verificationName, range)
    }
  }
}
