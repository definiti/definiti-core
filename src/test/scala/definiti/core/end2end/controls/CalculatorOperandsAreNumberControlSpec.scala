package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.CalculatorOperandsAreNumberControl

class CalculatorOperandsAreNumberControlSpec extends EndToEndSpec {
  import CalculatorOperandsAreNumberControlSpec._

  "Project.generatePublicAST" should "validate an expression when logical operands are both number" in {
    val output = processFile("controls.calculatorOperandsAreNumber.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression when a logical operand is not number" in {
    val output = processFile("controls.calculatorOperandsAreNumber.nominalInvalid", configuration)
    output shouldBe ko[Root]
  }

  it should "invalidate an expression when the left expression is not number" in {
    val output = processFile("controls.calculatorOperandsAreNumber.invalidLeft", configuration)
    output should beKo(
      CalculatorOperandsAreNumberControl.errorNotNumber(Constants.string, invalidLeftLocation(2, 3, 9))
    )
  }

  it should "invalidate an expression when the right expression is not number" in {
    val output = processFile("controls.calculatorOperandsAreNumber.invalidRight", configuration)
    output should beKo(
      CalculatorOperandsAreNumberControl.errorNotNumber(Constants.string, invalidRightLocation(2, 19, 32))
    )
  }

  it should "invalidate an expression when both left and right expressions are not number" in {
    val output = processFile("controls.calculatorOperandsAreNumber.invalidBoth", configuration)
    output should beKo(
      CalculatorOperandsAreNumberControl.errorNotNumber(Constants.boolean, invalidBothLocation(2, 3, 20)),
      CalculatorOperandsAreNumberControl.errorNotNumber(Constants.string, invalidBothLocation(2, 23, 36))
    )
  }

  it should "invalidate an invalid expression in a condition" in {
    val output = processFile("controls.calculatorOperandsAreNumber.invalidCondition", configuration)
    output should beKo(
      CalculatorOperandsAreNumberControl.errorNotNumber(Constants.boolean, invalidConditionLocation(2, 7, 24)),
      CalculatorOperandsAreNumberControl.errorNotNumber(Constants.string, invalidConditionLocation(2, 27, 40))
    )
  }

  it should "validate an expression where both operands are the same type alias of a number" in {
    val output = processFile("controls.calculatorOperandsAreNumber.validAliasType", configuration)
    output shouldBe ok[Root]
  }
}

object CalculatorOperandsAreNumberControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(CalculatorOperandsAreNumberControl)

  val invalidLeftLocation = LocationPath.control(CalculatorOperandsAreNumberControl, "invalidLeft")
  val invalidRightLocation = LocationPath.control(CalculatorOperandsAreNumberControl, "invalidRight")
  val invalidBothLocation = LocationPath.control(CalculatorOperandsAreNumberControl, "invalidBoth")
  val invalidConditionLocation = LocationPath.control(CalculatorOperandsAreNumberControl, "invalidCondition")
}

