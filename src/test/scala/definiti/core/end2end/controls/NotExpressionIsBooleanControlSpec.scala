package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.Constants
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.NotExpressionIsBooleanControl

class NotExpressionIsBooleanControlSpec extends EndToEndSpec {
  import NotExpressionIsBooleanControlSpec._

  "Project.generatePublicAST" should "validate an expression when inner type is boolean" in {
    val output = processFile("controls.notExpressionIsBoolean.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an expression when inner type is not boolean" in {
    val output = processFile("controls.notExpressionIsBoolean.nominalInvalid", configuration)
    output shouldBe ko[Root]
  }

  it should "invalidate an invalid expression in a condition" in {
    val output = processFile("controls.notExpressionIsBoolean.invalidCondition", configuration)
    output should beKo(
      NotExpressionIsBooleanControl.errorNotBoolean(Constants.integer, invalidConditionLocation(2, 8, 21))
    )
  }
}

object NotExpressionIsBooleanControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(NotExpressionIsBooleanControl)

  val invalidConditionLocation = LocationPath.control(NotExpressionIsBooleanControl, "invalidCondition")
}