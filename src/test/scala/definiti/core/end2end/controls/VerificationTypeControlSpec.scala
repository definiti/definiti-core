package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.VerificationTypeControl

class VerificationTypeControlSpec extends EndToEndSpec {

  import VerificationTypeControlSpec._

  "Project.generatePublicAST" should "validate a verification with a valid native type" in {
    val output = processFile("controls.verificationType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a verification with a valid defined type" in {
    val output = processFile("controls.verificationType.definedType", configuration)
    output shouldBe ok[Root]
  }

  // TODO: We need transparent types for it to work again.
  /*
  it should "validate a verification with a valid alias type" in {
    val output = processFile("controls.verificationType.aliasType", configuration)
    output shouldBe ok[Root]
  }
  */

  it should "invalidate a verification with an unknown type" in {
    val output = processFile("controls.verificationType.unknownType", configuration)
    output should beResult(Ko[Root](
      VerificationTypeControl.errorUnknownType("Unknown", unknownTypeLocation(3, 4, 20))
    ))
  }
}

object VerificationTypeControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(VerificationTypeControl)

  val unknownTypeLocation = LocationPath.control(VerificationTypeControl, "unknownType")
}
