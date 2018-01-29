package definiti.core.end2end.controls

import definiti.core.ProgramResultMatchers._
import definiti.core._
import definiti.core.ast.Root
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
  import EndToEndSpec._

  val configuration = configurationForceControls(VerificationIsBooleanControl.name)

  val numberSrc = "src/test/resources/samples/controls/verificationIsBoolean/number.def"
  val numberLocation = LocationPath(numberSrc)

  val conditionSrc = "src/test/resources/samples/controls/verificationIsBoolean/condition.def"
  val conditionLocation = LocationPath(conditionSrc)
}