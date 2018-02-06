package definiti.core.end2end.controls

import definiti.core.ProgramResultMatchers._
import definiti.core._
import definiti.core.ast.Root
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.VerificationIsOkKoControl

class VerificationIsOkKoControlSpec extends EndToEndSpec {
  import VerificationIsOkKoControlSpec._

  "Project.generatePublicAST" should "validate a verification returning OkKo with typed message" in {
    val output = processFile("controls.verificationIsOkKo.nominal", configuration)
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
  import EndToEndSpec._

  val configuration = configurationForceControls(VerificationIsOkKoControl.name)

  val numberLocation = LocationPath.control(VerificationIsOkKoControl.name, "number")
  val booleanLocation = LocationPath.control(VerificationIsOkKoControl.name, "boolean")
  val invalidTypesLocation = LocationPath.control(VerificationIsOkKoControl.name, "invalidTypes")
  val invalidNumberOfParametersLocation = LocationPath.control(VerificationIsOkKoControl.name, "invalidNumberOfParameters")
}