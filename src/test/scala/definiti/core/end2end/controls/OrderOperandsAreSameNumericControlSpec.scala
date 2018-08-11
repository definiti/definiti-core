package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.OrderOperandsAreSameNumericControl

class OrderOperandsAreSameNumericControlSpec extends EndToEndSpec {

  import OrderOperandsAreSameNumericControlSpec._

  "Project.generatePublicAST" should "validate an expression when logical operands are both boolean" in {
    val output = processFile("controls.orderOperandsAreSameNumeric.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression when a logical operand is not boolean" in {
    val output = processFile("controls.orderOperandsAreSameNumeric.nominalInvalid", configuration)
    output shouldBe ko[Root]
  }

  it should "invalidate an expression when the left expression is not boolean" in {
    val output = processFile("controls.orderOperandsAreSameNumeric.invalidLeft", configuration)
    output should beKo(
      OrderOperandsAreSameNumericControl.errorNotNumeric(Constants.string, invalidLeftLocation(2, 3, 9))
    )
  }

  it should "invalidate an expression when the right expression is not boolean" in {
    val output = processFile("controls.orderOperandsAreSameNumeric.invalidRight", configuration)
    output should beKo(
      OrderOperandsAreSameNumericControl.errorNotNumeric(Constants.string, invalidRightLocation(2, 20, 33))
    )
  }

  it should "invalidate an expression when both left and right expressions are not boolean" in {
    val output = processFile("controls.orderOperandsAreSameNumeric.invalidBoth", configuration)
    output should beKo(
      OrderOperandsAreSameNumericControl.errorNotNumeric(Constants.boolean, invalidBothLocation(2, 3, 20)),
      OrderOperandsAreSameNumericControl.errorNotNumeric(Constants.string, invalidBothLocation(2, 23, 36))
    )
  }

  it should "invalidate an invalid expression in a condition" in {
    val output = processFile("controls.orderOperandsAreSameNumeric.invalidCondition", configuration)
    output should beKo(
      OrderOperandsAreSameNumericControl.errorNotNumeric(Constants.boolean, invalidConditionLocation(2, 7, 24)),
      OrderOperandsAreSameNumericControl.errorNotNumeric(Constants.string, invalidConditionLocation(2, 27, 40))
    )
  }

  it should "invalidate an expression where both operands are not the same numeric type" in {
    val output = processFile("controls.orderOperandsAreSameNumeric.differentNumeric", configuration)
    output should beKo(
      OrderOperandsAreSameNumericControl.differentNumeric(Constants.number, Constants.integer, differentNumericLocation(2, 3, 20))
    )
  }
}

object OrderOperandsAreSameNumericControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(OrderOperandsAreSameNumericControl)

  val invalidLeftLocation = LocationPath.control(OrderOperandsAreSameNumericControl, "invalidLeft")
  val invalidRightLocation = LocationPath.control(OrderOperandsAreSameNumericControl, "invalidRight")
  val invalidBothLocation = LocationPath.control(OrderOperandsAreSameNumericControl, "invalidBoth")
  val invalidConditionLocation = LocationPath.control(OrderOperandsAreSameNumericControl, "invalidCondition")
  val differentNumericLocation = LocationPath.control(OrderOperandsAreSameNumericControl, "differentNumeric")
}

