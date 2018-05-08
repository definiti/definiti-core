package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
import definiti.core._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.VerificationIsOkKoControl

class VerificationIsOkKoControlSpec extends EndToEndSpec {
  import VerificationIsOkKoControlSpec._

  "Project.generatePublicAST" should "validate a verification returning OkKo with typed message" in {
    val output = processFile("controls.verificationIsOkKo.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a verification returning OkKo with typed message in a package" in {
    val output = processFile("controls.verificationIsOkKo.package", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a verification returning number" in {
    val output = processFile("controls.verificationIsOkKo.number", configuration)
    output should beKo(
      VerificationIsOkKoControl.errorNotOkKo("ShortString", Constants.number, numberLocation(4, 3, 6, 4))
    )
  }

  it should "invalidate a verification returning boolean" in {
    val output = processFile("controls.verificationIsOkKo.boolean", configuration)
    output should beKo(
      VerificationIsOkKoControl.errorNotOkKo("ShortString", Constants.boolean, booleanLocation(4, 3, 6, 4))
    )
  }

  it should "invalidate a KoValue with invalid number of parameters" in {
    val output = processFile("controls.verificationIsOkKo.invalidNumberOfParameters", configuration)
    output should beKo(
      VerificationIsOkKoControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(5, 5, 17))
    )
  }

  it should "invalidate a KoValue with invalid types of parameters" in {
    val output = processFile("controls.verificationIsOkKo.invalidTypes", configuration)
    output should beKo(
      VerificationIsOkKoControl.invalidType(Constants.string, Constants.number, invalidTypesLocation(5, 8, 11))
    )
  }
}

object VerificationIsOkKoControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(VerificationIsOkKoControl)

  val numberLocation = LocationPath.control(VerificationIsOkKoControl, "number")
  val booleanLocation = LocationPath.control(VerificationIsOkKoControl, "boolean")
  val invalidTypesLocation = LocationPath.control(VerificationIsOkKoControl, "invalidTypes")
  val invalidNumberOfParametersLocation = LocationPath.control(VerificationIsOkKoControl, "invalidNumberOfParameters")
}