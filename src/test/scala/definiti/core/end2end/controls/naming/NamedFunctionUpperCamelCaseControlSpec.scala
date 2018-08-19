package definiti.core.end2end.controls.naming

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers.{beResult, ok}
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.naming.NamedFunctionUpperCamelCaseControl

class NamedFunctionUpperCamelCaseControlSpec extends EndToEndSpec {

  import NamedFunctionUpperCamelCaseControlSpec._

  "Project.generatePublicAST" should "validate a named function with a valid upperCamelCame format" in {
    val output = processFile("controls.naming.namedFunctionUpperCamelCase.valid", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a named function with an invalid UpperCamelCame format" in {
    val output = processFile("controls.naming.namedFunctionUpperCamelCase.invalid", configuration)
    output should beResult(Ko[Root](
      NamedFunctionUpperCamelCaseControl.invalidUpperCamelCaseFormat("myNamedFunction", invalidLocation(1, 1, 3, 2))
    ))
  }
}

object NamedFunctionUpperCamelCaseControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(NamedFunctionUpperCamelCaseControl)

  val invalidLocation = LocationPath.controlNaming(NamedFunctionUpperCamelCaseControl, "invalid")
}