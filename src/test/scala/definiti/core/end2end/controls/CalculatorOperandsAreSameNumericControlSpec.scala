package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.CalculatorOperandsAreSameNumericControl

class CalculatorOperandsAreSameNumericControlSpec extends EndToEndSpec {
  import CalculatorOperandsAreSameNumericControlSpec._

  "Project.generatePublicAST" should "validate an expression when logical operands are both the same numeric" in {
    val output = processFile("controls.calculatorOperandsAreSameNumeric.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression when a logical operand is not numeric" in {
    val output = processFile("controls.calculatorOperandsAreSameNumeric.nominalInvalid", configuration)
    output shouldBe ko[Root]
  }

  it should "invalidate an expression when the left expression is not numeric" in {
    val output = processFile("controls.calculatorOperandsAreSameNumeric.invalidLeft", configuration)
    output should beKo(
      CalculatorOperandsAreSameNumericControl.errorNotNumeric(Constants.string, invalidLeftLocation(2, 3, 9))
    )
  }

  it should "invalidate an expression when the right expression is not numeric" in {
    val output = processFile("controls.calculatorOperandsAreSameNumeric.invalidRight", configuration)
    output should beKo(
      CalculatorOperandsAreSameNumericControl.errorNotNumeric(Constants.string, invalidRightLocation(2, 19, 32))
    )
  }

  it should "invalidate an expression when both left and right expressions are not numeric" in {
    val output = processFile("controls.calculatorOperandsAreSameNumeric.invalidBoth", configuration)
    output should beKo(
      CalculatorOperandsAreSameNumericControl.errorNotNumeric(Constants.boolean, invalidBothLocation(2, 3, 20)),
      CalculatorOperandsAreSameNumericControl.errorNotNumeric(Constants.string, invalidBothLocation(2, 23, 36))
    )
  }

  it should "invalidate an invalid expression in a condition" in {
    val output = processFile("controls.calculatorOperandsAreSameNumeric.invalidCondition", configuration)
    output should beKo(
      CalculatorOperandsAreSameNumericControl.errorNotNumeric(Constants.boolean, invalidConditionLocation(2, 7, 24)),
      CalculatorOperandsAreSameNumericControl.errorNotNumeric(Constants.string, invalidConditionLocation(2, 27, 40))
    )
  }

  it should "validate an expression where both operands are the same type alias of a numeric" in {
    val output = processFile("controls.calculatorOperandsAreSameNumeric.validAliasType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression where both operands are not the same numeric type" in {
    val output = processFile("controls.calculatorOperandsAreSameNumeric.differentNumeric", configuration)
    output should beKo(
      CalculatorOperandsAreSameNumericControl.differentNumeric(Constants.number, Constants.integer, differentNumericLocation(2, 3, 19))
    )
  }
}

object CalculatorOperandsAreSameNumericControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(CalculatorOperandsAreSameNumericControl)

  val invalidLeftLocation = LocationPath.control(CalculatorOperandsAreSameNumericControl, "invalidLeft")
  val invalidRightLocation = LocationPath.control(CalculatorOperandsAreSameNumericControl, "invalidRight")
  val invalidBothLocation = LocationPath.control(CalculatorOperandsAreSameNumericControl, "invalidBoth")
  val invalidConditionLocation = LocationPath.control(CalculatorOperandsAreSameNumericControl, "invalidCondition")
  val differentNumericLocation = LocationPath.control(CalculatorOperandsAreSameNumericControl, "differentNumeric")
}

