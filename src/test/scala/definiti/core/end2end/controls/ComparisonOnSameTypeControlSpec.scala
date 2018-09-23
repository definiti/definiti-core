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
      ComparisonOnSameTypeControl.errorDifferentTypes(Constants.integer, Constants.string, invalidConditionLocation(2, 7, 37))
    )
  }

  it should "invalidate a condition between a closed type and a type" in {
    val output = processFile("controls.comparisonOnSameType.invalidAliasType", configuration)
    output should beKo(
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Amount"), Constants.number, invalidAliasTypeLocation(4, 3, 15)),
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Amount"), Constants.number, invalidAliasTypeLocation(8, 3, 14))
    )
  }

  it should "validate a condition between a transparent type and a type" in {
    val output = processFile("controls.comparisonOnSameType.validTransparentType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a condition between two alias types" in {
    val output = processFile("controls.comparisonOnSameType.invalidDoubleAliasType", configuration)
    output should beKo(
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Amount"), TypeReference("Rate"), invalidDoubleAliasTypeLocation(5, 3, 16)),
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Amount"), TypeReference("Rate"), invalidDoubleAliasTypeLocation(9, 3, 15))
    )
  }

  it should "validate an expression between two attribute types" in {
    val output = processFile("controls.comparisonOnSameType.validAttributeType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression between an attribute type and another type" in {
    val output = processFile("controls.comparisonOnSameType.invalidAttributeType", configuration)
    output should beKo(
      ComparisonOnSameTypeControl.errorDifferentTypes(TypeReference("Station.Name"), TypeReference("String"), invalidAttributeTypeLocation(6, 3, 23))
    )
  }

  it should "validate an expression between a transparent attribute type and another type" in {
    val output = processFile("controls.comparisonOnSameType.validTransparentAttributeType", configuration)
    output shouldBe ok[Root]
  }
}

object ComparisonOnSameTypeControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(ComparisonOnSameTypeControl)

  val invalidConditionLocation = LocationPath.control(ComparisonOnSameTypeControl, "invalidCondition")
  val invalidAliasTypeLocation = LocationPath.control(ComparisonOnSameTypeControl, "invalidAliasType")
  val invalidDoubleAliasTypeLocation = LocationPath.control(ComparisonOnSameTypeControl, "invalidDoubleAliasType")
  val invalidAttributeTypeLocation = LocationPath.control(ComparisonOnSameTypeControl, "invalidAttributeType")
}