package definiti.core.end2end.controls

import definiti.common.ast.{Root, TypedMessage}
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
import definiti.core._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.TypeVerificationIsOkKoControl

class TypeVerificationIsOkKoControlSpec extends EndToEndSpec {
  import TypeVerificationIsOkKoControlSpec._

  "Project.generatePublicAST" should "validate a type verification returning OkKo with typed message" in {
    val output = processFile("controls.typeVerificationIsOkKo.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a type verification returning OkKo with typed message in a package" in {
    val output = processFile("controls.typeVerificationIsOkKo.package", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a type verification returning number" in {
    val output = processFile("controls.typeVerificationIsOkKo.number", configuration)
    output should beKo(
      TypeVerificationIsOkKoControl.errorNotOkKo(
        "MyString",
        TypedMessage("error.short", Seq(Constants.string), numberLocation(4, 5, 35)),
        Constants.number,
        numberLocation(5, 5, 7, 6)
      )
    )
  }

  it should "invalidate a type verification returning boolean" in {
    val output = processFile("controls.typeVerificationIsOkKo.boolean", configuration)
    output should beKo(
      TypeVerificationIsOkKoControl.errorNotOkKo(
        "ShortString",
        TypedMessage("error.short", Seq(Constants.string), numberLocation(4, 5, 35)),
        Constants.boolean,
        booleanLocation(5, 5, 7, 6)
      )
    )
  }

  it should "invalidate a KoValue with invalid number of parameters" in {
    val output = processFile("controls.typeVerificationIsOkKo.invalidNumberOfParameters", configuration)
    output should beKo(
      TypeVerificationIsOkKoControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(8, 7, 19))
    )
  }

  it should "invalidate a KoValue with invalid types of parameters" in {
    val output = processFile("controls.typeVerificationIsOkKo.invalidTypes", configuration)
    output should beKo(
      TypeVerificationIsOkKoControl.invalidType(Constants.string, Constants.number, invalidTypesLocation(6, 10, 13))
    )
  }
}

object TypeVerificationIsOkKoControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(TypeVerificationIsOkKoControl)

  val numberLocation = LocationPath.control(TypeVerificationIsOkKoControl, "number")
  val booleanLocation = LocationPath.control(TypeVerificationIsOkKoControl, "boolean")
  val invalidTypesLocation = LocationPath.control(TypeVerificationIsOkKoControl, "invalidTypes")
  val invalidNumberOfParametersLocation = LocationPath.control(TypeVerificationIsOkKoControl, "invalidNumberOfParameters")
}