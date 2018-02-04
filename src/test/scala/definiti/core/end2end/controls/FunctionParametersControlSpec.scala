package definiti.core.end2end.controls

import definiti.core.ProgramResultMatchers._
import definiti.core.ast.Root
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.FunctionParametersControl
import definiti.core.{Constants, Ko}

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
  import EndToEndSpec._

  val configuration = configurationForceControls(FunctionParametersControl.name)

  val invalidNumberOfParametersLocation = LocationPath("src/test/resources/samples/controls/functionParameters/invalidNumberOfParameters.def")
  val invalidTypeReferenceLocation = LocationPath("src/test/resources/samples/controls/functionParameters/invalidTypeReference.def")
}