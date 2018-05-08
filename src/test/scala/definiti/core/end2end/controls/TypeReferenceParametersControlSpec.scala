package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.TypeDeclarationParametersControl

class TypeReferenceParametersControlSpec extends EndToEndSpec {

  import TypeReferenceParametersControlSpec._

  "Project.generatePublicAST" should "validate a type referencing another type with valid parameters" in {
    val output = processFile("controls.typeDeclarationParameters.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a type referencing another type with valid parameters in package" in {
    val output = processFile("controls.typeDeclarationParameters.package", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a type referencing another type with transitive variables" in {
    val output = processFile("controls.typeDeclarationParameters.transitive", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a type referencing another type with different number of parameters" in {
    val output = processFile("controls.typeDeclarationParameters.invalidNumberOfParameters", configuration)
    output should beResult(Ko[Root](
      TypeDeclarationParametersControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(2, 9, 31)),
      TypeDeclarationParametersControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(3, 12, 25)),
      TypeDeclarationParametersControl.invalidNumberOfParameters(1, 2, invalidNumberOfParametersLocation(6, 27, 40))
    ))
  }

  it should "invalidate a type referencing another type with invalid type parameters" in {
    val output = processFile("controls.typeDeclarationParameters.invalidTypeOfParameters", configuration)
    output should beResult(Ko[Root](
      TypeDeclarationParametersControl.invalidParameterType(Constants.number, Constants.string, invalidTypeOfParametersLocation(2, 24, 33)),
      TypeDeclarationParametersControl.invalidParameterType(Constants.number, Constants.string, invalidTypeOfParametersLocation(3, 20, 29)),
      TypeDeclarationParametersControl.invalidParameterType(Constants.number, Constants.string, invalidTypeOfParametersLocation(6, 35, 44))
    ))
  }
}

object TypeReferenceParametersControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(TypeDeclarationParametersControl)

  val invalidNumberOfParametersLocation = LocationPath.control(TypeDeclarationParametersControl, "invalidNumberOfParameters")
  val invalidTypeOfParametersLocation = LocationPath.control(TypeDeclarationParametersControl, "invalidTypeOfParameters")
}
