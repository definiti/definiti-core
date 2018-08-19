package definiti.core.end2end.controls.naming

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers.{beResult, ok}
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.naming.TypeUpperCamelCaseControl

class TypeUpperCamelCaseControlSpec extends EndToEndSpec {

  import TypeUpperCamelCaseControlSpec._

  "Project.generatePublicAST" should "validate a type with a valid UpperCamelCame format" in {
    val output = processFile("controls.naming.typeUpperCamelCase.valid", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a type with an invalid UpperCamelCame format" in {
    val output = processFile("controls.naming.typeUpperCamelCase.invalid", configuration)
    output should beResult(Ko[Root](
      TypeUpperCamelCaseControl.invalidUpperCamelCaseFormat("myAlias", invalidLocation(1, 1, 22)),
      TypeUpperCamelCaseControl.invalidUpperCamelCaseFormat("myDefined", invalidLocation(3, 1, 5, 2)),
      TypeUpperCamelCaseControl.invalidUpperCamelCaseFormat("myString", invalidLocation(4, 3, 27)),
      TypeUpperCamelCaseControl.invalidUpperCamelCaseFormat("myEnum", invalidLocation(7, 1, 10, 2))
    ))
  }
}

object TypeUpperCamelCaseControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(TypeUpperCamelCaseControl)

  val invalidLocation = LocationPath.controlNaming(TypeUpperCamelCaseControl, "invalid")
}