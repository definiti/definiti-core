package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.LogicalOperandsAreBooleanControl

class LogicalOperandsAreBooleanControlSpec extends EndToEndSpec {
  import LogicalOperandsAreBooleanControlSpec._

  "Project.generatePublicAST" should "validate an expression when logical operands are both boolean" in {
    val output = processFile("controls.logicalOperandsAreBoolean.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression when a logical operand is not boolean" in {
    val output = processFile("controls.logicalOperandsAreBoolean.nominalInvalid", configuration)
    output shouldBe ko[Root]
  }

  it should "invalidate an expression when the left expression is not boolean" in {
    val output = processFile("controls.logicalOperandsAreBoolean.invalidLeft", configuration)
    output should beKo(
      LogicalOperandsAreBooleanControl.errorNotBoolean(Constants.number, invalidLeftLocation(2, 3, 16))
    )
  }

  it should "invalidate an expression when the right expression is not boolean" in {
    val output = processFile("controls.logicalOperandsAreBoolean.invalidRight", configuration)
    output should beKo(
      LogicalOperandsAreBooleanControl.errorNotBoolean(Constants.string, invalidRightLocation(2, 24, 37))
    )
  }

  it should "invalidate an expression when both left and right expressions are not boolean" in {
    val output = processFile("controls.logicalOperandsAreBoolean.invalidBoth", configuration)
    output should beKo(
      LogicalOperandsAreBooleanControl.errorNotBoolean(Constants.number, invalidBothLocation(2, 3, 16)),
      LogicalOperandsAreBooleanControl.errorNotBoolean(Constants.string, invalidBothLocation(2, 20, 33))
    )
  }

  it should "invalidate an invalid expression in a condition" in {
    val output = processFile("controls.logicalOperandsAreBoolean.invalidCondition", configuration)
    output should beKo(
      LogicalOperandsAreBooleanControl.errorNotBoolean(Constants.number, invalidConditionLocation(2, 7, 20)),
      LogicalOperandsAreBooleanControl.errorNotBoolean(Constants.string, invalidConditionLocation(2, 24, 37))
    )
  }
}

object LogicalOperandsAreBooleanControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(LogicalOperandsAreBooleanControl.name)

  val invalidLeftLocation = LocationPath.control(LogicalOperandsAreBooleanControl.name, "invalidLeft")
  val invalidRightLocation = LocationPath.control(LogicalOperandsAreBooleanControl.name, "invalidRight")
  val invalidBothLocation = LocationPath.control(LogicalOperandsAreBooleanControl.name, "invalidBoth")
  val invalidConditionLocation = LocationPath.control(LogicalOperandsAreBooleanControl.name, "invalidCondition")
}