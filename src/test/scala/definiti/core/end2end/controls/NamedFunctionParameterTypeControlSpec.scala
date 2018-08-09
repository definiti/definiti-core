package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.NamedFunctionParameterTypeControl

class NamedFunctionParameterTypeControlSpec extends EndToEndSpec {

  import NamedFunctionParameterTypeControlSpec._

  "Project.generatePublicAST" should "validate a valid parameter type" in {
    val output = processFile("controls.namedFunctionParameterType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an unknown parameter type" in {
    val output = processFile("controls.namedFunctionParameterType.invalidTypeReference", configuration)
    output should beResult(Ko[Root](
      NamedFunctionParameterTypeControl.errorUnknownType("Unknown", invalidTypeReferenceLocation(1, 13, 28))
    ))
  }

  it should "validate a parameter with a type referencing a valid attribute type" in {
    val output = processFile("controls.namedFunctionParameterType.validAttributeTypeReference", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a parameter with a type referencing an unknown attribute type" in {
    val output = processFile("controls.namedFunctionParameterType.invalidAttributeTypeReference", configuration)
    output should beResult(Ko[Root](
      NamedFunctionParameterTypeControl.errorUnknownType("User.Unknown", invalidAttributeTypeReferenceLocation(5, 20, 42))
    ))
  }
}

object NamedFunctionParameterTypeControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(NamedFunctionParameterTypeControl)

  val invalidTypeReferenceLocation = LocationPath.control(NamedFunctionParameterTypeControl, "invalidTypeReference")
  val invalidAttributeTypeReferenceLocation = LocationPath.control(NamedFunctionParameterTypeControl, "invalidAttributeTypeReference")
}