package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
import definiti.core._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.VerificationIsBooleanControl

class VerificationIsBooleanControlSpec extends EndToEndSpec {
  import VerificationIsBooleanControlSpec._

  "Project.generatePublicAST" should "validate a verification returning boolean" in {
    val output = processFile("controls.verificationIsBoolean.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a verification returning number" in {
    val expected = Ko[Root](
      VerificationIsBooleanControl.errorNotBoolean("InvalidNumber", Constants.number, numberLocation(3, 3, 5, 4))
    )
    val output = processFile("controls.verificationIsBoolean.number", configuration)
    output should beResult(expected)
  }

  it should "invalidate a verification when condition does not return a boolean on each branch" in {
    val expected = Ko[Root](
      VerificationIsBooleanControl.errorNotBoolean("InvalidCondition", Constants.unit, conditionLocation(3, 3, 9, 4))
    )
    val output = processFile("controls.verificationIsBoolean.condition", configuration)
    output should beResult(expected)
  }
}

object VerificationIsBooleanControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(VerificationIsBooleanControl)

  val numberLocation = LocationPath.control(VerificationIsBooleanControl, "number")
  val conditionLocation = LocationPath.control(VerificationIsBooleanControl, "condition")
}