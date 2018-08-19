package definiti.core.end2end.controls.naming

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers.{beResult, ok}
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.naming.VerificationLowerCamelCaseControl

class VerificationLowerCamelCaseControlSpec extends EndToEndSpec {

  import VerificationLowerCamelCaseControlSpec._

  "Project.generatePublicAST" should "validate a verification with a valid lowerCamelCame format" in {
    val output = processFile("controls.naming.verificationLowerCamelCase.valid", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a verification with an invalid lowerCamelCame format" in {
    val output = processFile("controls.naming.verificationLowerCamelCase.invalid", configuration)
    output should beResult(Ko[Root](
      VerificationLowerCamelCaseControl.invalidLowerCamelCaseFormat("IsNonEmpty", invalidLocation(1, 1, 6, 2))
    ))
  }
}

object VerificationLowerCamelCaseControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(VerificationLowerCamelCaseControl)

  val invalidLocation = LocationPath.controlNaming(VerificationLowerCamelCaseControl, "invalid")
}