package definiti.core.end2end.controls.naming

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers.{beResult, ok}
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.naming.TypeLowerCamelCaseControl

class TypeLowerCamelCaseControlSpec extends EndToEndSpec {

  import TypeLowerCamelCaseControlSpec._

  "Project.generatePublicAST" should "validate a type with a valid lowerCamelCame format" in {
    val output = processFile("controls.naming.typeLowerCamelCase.valid", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a type with an invalid lowerCamelCame format" in {
    val output = processFile("controls.naming.typeLowerCamelCase.invalid", configuration)
    output should beResult(Ko[Root](
      TypeLowerCamelCaseControl.invalidLowerCamelCaseFormat("MyAlias", invalidLocation(1, 1, 22)),
      TypeLowerCamelCaseControl.invalidLowerCamelCaseFormat("MyDefined", invalidLocation(3, 1, 5, 2)),
      TypeLowerCamelCaseControl.invalidLowerCamelCaseFormat("MyString", invalidLocation(4, 3, 27)),
      TypeLowerCamelCaseControl.invalidLowerCamelCaseFormat("MyEnum", invalidLocation(7, 1, 10, 2))
    ))
  }
}

object TypeLowerCamelCaseControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(TypeLowerCamelCaseControl)

  val invalidLocation = LocationPath.controlNaming(TypeLowerCamelCaseControl, "invalid")
}