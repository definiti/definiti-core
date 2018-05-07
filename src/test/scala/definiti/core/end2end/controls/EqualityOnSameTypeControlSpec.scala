package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.EqualityOnSameTypeControl

class EqualityOnSameTypeControlSpec extends EndToEndSpec {
  import EqualityOnSameTypeControlSpec._

  "Project.generatePublicAST" should "validate an expression when equality operands are both the same typoe" in {
    val output = processFile("controls.equalityOnSameType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression when equality operands are not the same type" in {
    val output = processFile("controls.equalityOnSameType.nominalInvalid", configuration)
    output shouldBe ko[Root]
  }

  it should "invalidate an invalid expression in a condition" in {
    val output = processFile("controls.equalityOnSameType.invalidCondition", configuration)
    output should beKo(
      EqualityOnSameTypeControl.errorDifferentTypes(Constants.number, Constants.string, invalidConditionLocation(2, 7, 37))
    )
  }
}

object EqualityOnSameTypeControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(EqualityOnSameTypeControl.name)

  val invalidConditionLocation = LocationPath.control(EqualityOnSameTypeControl.name, "invalidCondition")
}