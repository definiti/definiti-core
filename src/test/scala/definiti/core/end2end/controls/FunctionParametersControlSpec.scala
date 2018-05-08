package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.FunctionParametersControl

class FunctionParametersControlSpec extends EndToEndSpec {
  import FunctionParametersControlSpec._

  "Project.generatePublicAST" should "validate a function call when parameters are valid" in {
    val output = processFile("controls.functionParameters.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a function call when the number of parameters does not match" in {
    val output = processFile("controls.functionParameters.invalidNumberOfParameters", configuration)
    output should beResult(Ko[Root](
      FunctionParametersControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(6, 3, 21))
    ))
  }

  it should "invalidate a function call when the type reference does not match" in {
    val output = processFile("controls.functionParameters.invalidTypeReference", configuration)
    output should beResult(Ko[Root](
      FunctionParametersControl.invalidParameterType(Constants.string, Constants.number, invalidTypeReferenceLocation(6, 11, 17))
    ))
  }
}

object FunctionParametersControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(FunctionParametersControl)

  val invalidNumberOfParametersLocation = LocationPath.control(FunctionParametersControl, "invalidNumberOfParameters")
  val invalidTypeReferenceLocation = LocationPath.control(FunctionParametersControl, "invalidTypeReference")
}