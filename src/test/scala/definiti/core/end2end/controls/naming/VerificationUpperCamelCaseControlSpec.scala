package definiti.core.end2end.controls.naming

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers.{beResult, ok}
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.naming.VerificationUpperCamelCaseControl

class VerificationUpperCamelCaseControlSpec extends EndToEndSpec {

  import VerificationUpperCamelCaseControlSpec._

  "Project.generatePublicAST" should "validate a verification with a valid UpperCamelCame format" in {
    val output = processFile("controls.naming.verificationUpperCamelCase.valid", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a verification with an invalid UpperCamelCame format" in {
    val output = processFile("controls.naming.verificationUpperCamelCase.invalid", configuration)
    output should beResult(Ko[Root](
      VerificationUpperCamelCaseControl.invalidUpperCamelCaseFormat("isNonEmpty", invalidLocation(1, 1, 6, 2))
    ))
  }
}

object VerificationUpperCamelCaseControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(VerificationUpperCamelCaseControl)

  val invalidLocation = LocationPath.controlNaming(VerificationUpperCamelCaseControl, "invalid")
}