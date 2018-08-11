package definiti.core.end2end.controls

import definiti.common.ast.{LambdaReference, NamedFunctionReference, Root, TypeReference}
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.MethodParametersControl

class MethodParametersControlSpec extends EndToEndSpec {
  import MethodParametersControlSpec._

  "Project.generatePublicAST" should "validate a method call when parameters are valid" in {
    val output = processFile("controls.methodParameters.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a method call with a lambda and generics" in {
    val output = processFile("controls.methodParameters.validGenericLambdaReference", configuration)
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
      MethodParametersControl.invalidParameterType(Constants.string, Constants.integer, invalidTypeReferenceLocation(2, 21, 24))
    ))
  }

  it should "invalidate a function call when the type reference targets the alias type and not the type" in {
    val output = processFile("controls.methodParameters.invalidAliasTypeReference", configuration)
    output should beResult(Ko[Root](
      MethodParametersControl.invalidParameterType(Constants.string, TypeReference("Substring"), invalidAliasTypeReferenceLocation(4, 24, 33))
    ))
  }
}

object MethodParametersControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(MethodParametersControl)

  val invalidLambdaReferenceLocation = LocationPath.control(MethodParametersControl, "invalidLambdaReference")
  val invalidNamedFunctionReferenceLocation = LocationPath.control(MethodParametersControl, "invalidNamedFunctionReference")
  val invalidNumberOfParametersLocation = LocationPath.control(MethodParametersControl, "invalidNumberOfParameters")
  val invalidTypeReferenceLocation = LocationPath.control(MethodParametersControl, "invalidTypeReference")
  val invalidAliasTypeReferenceLocation = LocationPath.control(MethodParametersControl, "invalidAliasTypeReference")
}