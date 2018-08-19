package definiti.core.end2end.controls.naming

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers.{ok, _}
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.naming.NamedFunctionLowerCamelCaseControl

class NamedFunctionLowerCamelCaseControlSpec extends EndToEndSpec {

  import NamedFunctionLowerCamelCaseControlSpec._

  "Project.generatePublicAST" should "validate a named function with a valid lowerCamelCame format" in {
    val output = processFile("controls.naming.namedFunctionLowerCamelCase.valid", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a named function with an invalid lowerCamelCame format" in {
    val output = processFile("controls.naming.namedFunctionLowerCamelCase.invalid", configuration)
    output should beResult(Ko[Root](
      NamedFunctionLowerCamelCaseControl.invalidLowerCamelCaseFormat("MyNamedFunction", invalidLocation(1, 1, 3, 2))
    ))
  }

}

object NamedFunctionLowerCamelCaseControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(NamedFunctionLowerCamelCaseControl)

  val invalidLocation = LocationPath.controlNaming(NamedFunctionLowerCamelCaseControl, "invalid")
}