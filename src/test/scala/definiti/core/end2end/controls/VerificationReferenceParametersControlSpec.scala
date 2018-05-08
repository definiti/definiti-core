package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.VerificationReferenceParametersControl

class VerificationReferenceParametersControlSpec extends EndToEndSpec {

  import VerificationReferenceParametersControlSpec._

  "Project.generatePublicAST" should "validate a type referencing a control with valid parameters" in {
    val output = processFile("controls.verificationReferenceParameters.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a type referencing a control with valid parameters in package" in {
    val output = processFile("controls.verificationReferenceParameters.package", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a type referencing a control with valid parameters in package and another String parameter for replacing the message" in {
    val output = processFile("controls.verificationReferenceParameters.acceptAnotherStringParameter", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a type referencing a control with different number of parameters" in {
    val output = processFile("controls.verificationReferenceParameters.invalidNumberOfParameters", configuration)
    output should beResult(Ko[Root](
      VerificationReferenceParametersControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(1, 14, 46)),
      VerificationReferenceParametersControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(2, 16, 45)),
      VerificationReferenceParametersControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(6, 29, 61))
    ))
  }

  it should "invalidate a type referencing a control with invalid type parameters" in {
    val output = processFile("controls.verificationReferenceParameters.invalidTypeOfParameters", configuration)
    output should beResult(Ko[Root](
      VerificationReferenceParametersControl.invalidParameterType(Constants.number, Constants.string, invalidTypeOfParametersLocation(1, 41, 50)),
      VerificationReferenceParametersControl.invalidParameterType(Constants.number, Constants.string, invalidTypeOfParametersLocation(2, 40, 49)),
      VerificationReferenceParametersControl.invalidParameterType(Constants.number, Constants.string, invalidTypeOfParametersLocation(6, 56, 65))
    ))
  }
}

object VerificationReferenceParametersControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(VerificationReferenceParametersControl)

  val invalidNumberOfParametersLocation = LocationPath.control(VerificationReferenceParametersControl, "invalidNumberOfParameters")
  val invalidTypeOfParametersLocation = LocationPath.control(VerificationReferenceParametersControl, "invalidTypeOfParameters")
}
