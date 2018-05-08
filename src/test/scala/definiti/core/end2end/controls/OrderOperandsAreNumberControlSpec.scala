package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.OrderOperandsAreNumberControl

class OrderOperandsAreNumberControlSpec extends EndToEndSpec {
  import OrderOperandsAreNumberControlSpec._

  "Project.generatePublicAST" should "validate an expression when logical operands are both boolean" in {
    val output = processFile("controls.orderOperandsAreNumber.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression when a logical operand is not boolean" in {
    val output = processFile("controls.orderOperandsAreNumber.nominalInvalid", configuration)
    output shouldBe ko[Root]
  }

  it should "invalidate an expression when the left expression is not boolean" in {
    val output = processFile("controls.orderOperandsAreNumber.invalidLeft", configuration)
    output should beKo(
      OrderOperandsAreNumberControl.errorNotNumber(Constants.string, invalidLeftLocation(2, 3, 9))
    )
  }

  it should "invalidate an expression when the right expression is not boolean" in {
    val output = processFile("controls.orderOperandsAreNumber.invalidRight", configuration)
    output should beKo(
      OrderOperandsAreNumberControl.errorNotNumber(Constants.string, invalidRightLocation(2, 20, 33))
    )
  }

  it should "invalidate an expression when both left and right expressions are not boolean" in {
    val output = processFile("controls.orderOperandsAreNumber.invalidBoth", configuration)
    output should beKo(
      OrderOperandsAreNumberControl.errorNotNumber(Constants.boolean, invalidBothLocation(2, 3, 20)),
      OrderOperandsAreNumberControl.errorNotNumber(Constants.string, invalidBothLocation(2, 23, 36))
    )
  }

  it should "invalidate an invalid expression in a condition" in {
    val output = processFile("controls.orderOperandsAreNumber.invalidCondition", configuration)
    output should beKo(
      OrderOperandsAreNumberControl.errorNotNumber(Constants.boolean, invalidConditionLocation(2, 7, 24)),
      OrderOperandsAreNumberControl.errorNotNumber(Constants.string, invalidConditionLocation(2, 27, 40))
    )
  }
}

object OrderOperandsAreNumberControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(OrderOperandsAreNumberControl)

  val invalidLeftLocation = LocationPath.control(OrderOperandsAreNumberControl, "invalidLeft")
  val invalidRightLocation = LocationPath.control(OrderOperandsAreNumberControl, "invalidRight")
  val invalidBothLocation = LocationPath.control(OrderOperandsAreNumberControl, "invalidBoth")
  val invalidConditionLocation = LocationPath.control(OrderOperandsAreNumberControl, "invalidCondition")
}

