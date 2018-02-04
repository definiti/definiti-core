package definiti.core.end2end.controls

import definiti.core.Ko
import definiti.core.ProgramResultMatchers._
import definiti.core.ast.Root
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

  it should "validate a verification with a valid alias type" in {
    val output = processFile("controls.verificationType.aliasType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a verification with an unknown type" in {
    val output = processFile("controls.verificationType.unknownType", configuration)
    output should beResult(Ko[Root](
      VerificationTypeControl.errorUnknownType("Unknown", "UnknownVerification", unknownTypeLocation(3, 4, 20))
    ))
  }
}

object VerificationTypeControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(VerificationTypeControl.name)

  val unknownTypeLocation = LocationPath.control(VerificationTypeControl.name, "unknownType")
}
