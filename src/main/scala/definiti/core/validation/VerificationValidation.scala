package definiti.core.validation

import definiti.core.ast._
import definiti.core.{Invalid, Valid, Validation}

private[core] trait VerificationValidation {
  self: ASTValidation =>

  protected def validateVerification(verification: Verification): Validation = {
    validateDeepBooleanExpression(verification.function.body)
  }

  protected def validateVerificationReference(verification: VerificationReference, location: Location): Validation = {
    if (library.verificationsMap.contains(verification.verificationName)) {
      Valid
    } else {
      Invalid("Undefined verification: " + verification.verificationName, location)
    }
  }
}
