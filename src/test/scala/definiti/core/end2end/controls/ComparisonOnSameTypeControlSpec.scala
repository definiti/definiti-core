package definiti.core.end2end.controls

import definiti.common.ast.{Root, TypeReference}
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.ComparisonOnSameTypeControl

class ComparisonOnSameTypeControlSpec extends EndToEndSpec {
  import ComparisonOnSameTypeControlSpec._

  "Project.generatePublicAST" should "validate an expression when equality operands are both the same typoe" in {
    val output = processFile("controls.comparisonOnSameType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression when equality operands are not the same type" in {
    val output = processFile("controls.comparisonOnSameType.nominalInvalid", configuration)
    output shouldBe ko[Root]
  }

  it should "invalidate an invalid expression in a condition" in {
    val output = processFile("controls.comparisonOnSameType.invalidCondition", configuration)
    output should beKo(
      ComparisonOnSameTypeControl.errorDifferentTypes(Constants.number, Constants.string, invalidConditionLocation(2, 7, 37))
    )
  }

  it should "invalidate a condition between an alias type and a type" in {
    val output = processFile("controls.comparisonOnSameType.invalidAliasType", configuration)
    output should beKo(
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Amount"), Constants.number, invalidAliasTypeLocation(4, 3, 13)),
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Amount"), Constants.number, invalidAliasTypeLocation(8, 3, 12))
    )
  }

  it should "invalidate a condition between two alias types" in {
    val output = processFile("controls.comparisonOnSameType.invalidDoubleAliasType", configuration)
    output should beKo(
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Amount"), TypeReference("Rate"), invalidDoubleAliasTypeLocation(5, 3, 16)),
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Amount"), TypeReference("Rate"), invalidDoubleAliasTypeLocation(9, 3, 15))
    )
  }
}

object ComparisonOnSameTypeControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(ComparisonOnSameTypeControl)

  val invalidConditionLocation = LocationPath.control(ComparisonOnSameTypeControl, "invalidCondition")
  val invalidAliasTypeLocation = LocationPath.control(ComparisonOnSameTypeControl, "invalidAliasType")
  val invalidDoubleAliasTypeLocation = LocationPath.control(ComparisonOnSameTypeControl, "invalidDoubleAliasType")
}