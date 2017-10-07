package definiti.core

import org.scalatest.matchers.{MatchResult, Matcher}

trait ValidationMatchers {

  class ValidationMatcher(expected: Validation) extends Matcher[Validation] {

    def apply(left: Validation): MatchResult = {
      (expected, left) match {
        case (Valid, Valid) => success
        case (Valid, Invalid(_)) => failed(expected, left)
        case (Invalid(_), Valid) => failed(expected, left)
        case (Invalid(expectedErrors), Invalid(gotErrors)) =>
          val missingErrors = missingExpectedErrors(expectedErrors, gotErrors)
          val additionalErrors = additionalGotErrors(expectedErrors, gotErrors)
          if (missingErrors.nonEmpty || additionalErrors.nonEmpty) {
            failed(expected, left)
          } else {
            success
          }
      }
    }

    private def missingExpectedErrors(expectedErrors: Seq[Error], gotErrors: Seq[Error]): Seq[Error] = {
      expectedErrors.filter(expectedError => !gotErrors.contains(expectedError))
    }

    private def additionalGotErrors(expectedErrors: Seq[Error], gotErrors: Seq[Error]): Seq[Error] = {
      gotErrors.filter(gotError => !expectedErrors.contains(gotError))
    }

    private val success = MatchResult(matches = true, "", "")

    private def failed(expected: Validation, got: Validation) = MatchResult(matches = false, s"${expected.prettyPrint} did not equal ${got.prettyPrint}", "")
  }

  class ValidatedMatcher[A](expected: Validated[A]) extends Matcher[Validated[A]] {

    def apply(left: Validated[A]): MatchResult = {
      (expected, left) match {
        case (ValidValue(expectedValue), ValidValue(gotValue)) =>
          if (expectedValue == gotValue) {
            success
          } else {
            failed(expected, left)
          }
        case (Valid, Invalid(_)) => failed(expected, left)
        case (Invalid(_), Valid) => failed(expected, left)
        case (Invalid(expectedErrors), Invalid(gotErrors)) =>
          val missingErrors = missingExpectedErrors(expectedErrors, gotErrors)
          val additionalErrors = additionalGotErrors(expectedErrors, gotErrors)
          if (missingErrors.nonEmpty || additionalErrors.nonEmpty) {
            failed(expected, left)
          } else {
            success
          }
      }
    }

    private def missingExpectedErrors(expectedErrors: Seq[Error], gotErrors: Seq[Error]): Seq[Error] = {
      expectedErrors.filter(expectedError => !gotErrors.contains(expectedError))
    }

    private def additionalGotErrors(expectedErrors: Seq[Error], gotErrors: Seq[Error]): Seq[Error] = {
      gotErrors.filter(gotError => !expectedErrors.contains(gotError))
    }

    private val success = MatchResult(matches = true, "", "")

    private def failed(expected: Validated[A], got: Validated[A]) = MatchResult(matches = false, s"${expected.prettyPrint} did not equal ${got.prettyPrint}", "")
  }

  def beValidation(expected: Validation) = new ValidationMatcher(expected)

  def beValidated[A](expected: Validated[A]) = new ValidatedMatcher[A](expected)
}

object ValidationMatchers extends ValidationMatchers