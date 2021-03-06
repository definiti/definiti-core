package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.TopLevelFullNameUniquenessControl

class TopLevelFullNameUniquenessControlSpec extends EndToEndSpec {
  import TopLevelFullNameUniquenessControlSpec._

  "Project.generatePublicAST" should "validate a project when there is no duplication" in {
    val output = processFile("controls.topLevelFullNameUniqueness.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a project when two defined types have the same name" in {
    val output = processFile("controls.topLevelFullNameUniqueness.invalidTwoDefinedTypes", configuration)
    output should beKo(
      TopLevelFullNameUniquenessControl.errorSameName(
        "Duplicated",
        invalidTwoDefinedTypesLocation(1, 1, 3, 2),
        invalidTwoDefinedTypesLocation(5, 1, 7, 2)
      )
    )
  }

  it should "invalidate a project when a defined type and a named function have the same name" in {
    val output = processFile("controls.topLevelFullNameUniqueness.invalidDefinedTypeNamedFunction", configuration)
    output should beKo(
      TopLevelFullNameUniquenessControl.errorSameName(
        "Duplicated",
        invalidDefinedTypeNamedFunctionLocation(1, 1, 3, 2),
        invalidDefinedTypeNamedFunctionLocation(5, 1, 7, 2)
      )
    )
  }

  it should "invalidate a project when a named function and a verification have the same name" in {
    val output = processFile("controls.topLevelFullNameUniqueness.invalidNamedFunctionVerification", configuration)
    output should beKo(
      TopLevelFullNameUniquenessControl.errorSameName(
        "Duplicated",
        invalidNamedFunctionVerificationLocation(1, 1, 3, 2),
        invalidNamedFunctionVerificationLocation(5, 1, 10, 2)
      )
    )
  }

  it should "invalidate a project when a verification and an alias type have the same name" in {
    val output = processFile("controls.topLevelFullNameUniqueness.invalidVerificationAliasType", configuration)
    output should beKo(
      TopLevelFullNameUniquenessControl.errorSameName(
        "Duplicated",
        invalidVerificationAliasTypeLocation(1, 1, 6, 2),
        invalidVerificationAliasTypeLocation(8, 1, 25)
      )
    )
  }
}

object TopLevelFullNameUniquenessControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(TopLevelFullNameUniquenessControl)

  val invalidTwoDefinedTypesLocation = LocationPath.control(TopLevelFullNameUniquenessControl, "invalidTwoDefinedTypes")
  val invalidDefinedTypeNamedFunctionLocation = LocationPath.control(TopLevelFullNameUniquenessControl, "invalidDefinedTypeNamedFunction")
  val invalidNamedFunctionVerificationLocation = LocationPath.control(TopLevelFullNameUniquenessControl, "invalidNamedFunctionVerification")
  val invalidVerificationAliasTypeLocation = LocationPath.control(TopLevelFullNameUniquenessControl, "invalidVerificationAliasType")
}