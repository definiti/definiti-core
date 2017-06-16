package definiti.core.validation

import definiti.core.generators.VerificationGenerator
import definiti.core.parser.project.CoreParser
import definiti.core.{BooleanValue, Invalid, NumberValue, Valid}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ASTValidationVerificationSpec extends FlatSpec with Matchers with PropertyChecks with CoreParser {
  "ASTValidation.validateVerification" should "accept a function returning a Boolean" in {
    implicit val context = coreContext
    forAll(VerificationGenerator.anyVerification) { verification =>
      val normalizedVerification = verification.copy(
        function = verification.function.copy(
          body = BooleanValue(value = true, range = verification.function.body.range)
        )
      )

      ASTValidation.validateVerification(normalizedVerification) should be(Valid)
    }
  }

  it should "refuse a function returning a number" in {
    implicit val context = coreContext
    forAll(VerificationGenerator.anyVerification) { verification =>
      val normalizedVerification = verification.copy(
        function = verification.function.copy(
          body = NumberValue(value = 1, range = verification.function.body.range)
        )
      )

      ASTValidation.validateVerification(normalizedVerification) should be(an[Invalid])
    }
  }
}
