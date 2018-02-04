package definiti.core.end2end.controls

import definiti.core.ProgramResultMatchers._
import definiti.core.ast.{LambdaReference, NamedFunctionReference, Root}
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.MethodParametersControl
import definiti.core.{Constants, Ko}

class MethodParametersControlSpec extends EndToEndSpec {
  import MethodParametersControlSpec._

  "Project.generatePublicAST" should "validate a method call when parameters are valid" in {
    val output = processFile("controls.methodParameters.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a method call when lambda reference does not match" in {
    val output = processFile("controls.methodParameters.invalidLambdaReference", configuration)
    output should beResult(Ko[Root](
      MethodParametersControl.errorTypeEquality(Constants.string, Constants.number, invalidLambdaReferenceLocation(2, 16, 30))
    ))
  }

  it should "invalidate a method call when named function does not match" in {
    val output = processFile("controls.methodParameters.invalidNamedFunctionReference", configuration)
    output should beResult(Ko[Root](
      MethodParametersControl.errorTypeEquality(
        expected = LambdaReference(Seq(Constants.number), Constants.boolean),
        got = NamedFunctionReference("startsWithPrefix"),
        location = invalidNamedFunctionReferenceLocation(6, 15, 31)
      )
    ))
  }

  it should "invalidate a method call when the number of parameters does not match" in {
    val output = processFile("controls.methodParameters.invalidNumberOfParameters", configuration)
    output should beResult(Ko[Root](
      MethodParametersControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(2, 3, 35))
    ))
  }

  it should "invalidate a method call when the type reference does not match" in {
    val output = processFile("controls.methodParameters.invalidTypeReference", configuration)
    output should beResult(Ko[Root](
      MethodParametersControl.invalidParameterType(Constants.string, Constants.number, invalidTypeReferenceLocation(2, 21, 24))
    ))
  }
}

object MethodParametersControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(MethodParametersControl.name)

  val invalidLambdaReferenceLocation = LocationPath.control(MethodParametersControl.name, "invalidLambdaReference")
  val invalidNamedFunctionReferenceLocation = LocationPath.control(MethodParametersControl.name, "invalidNamedFunctionReference")
  val invalidNumberOfParametersLocation = LocationPath.control(MethodParametersControl.name, "invalidNumberOfParameters")
  val invalidTypeReferenceLocation = LocationPath.control(MethodParametersControl.name, "invalidTypeReference")
}