package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.TypeVerificationParameterUsableControl

class TypeVerificationParameterUsableControlSpec extends EndToEndSpec {
  import TypeVerificationParameterUsableControlSpec._

  "Project.generatePublicAST" should "validate a type verification with valid parameters" in {
    val output = processFile("controls.typeVerificationParameterUsable.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a type verification with valid parameters in a package" in {
    val output = processFile("controls.typeVerificationParameterUsable.package", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a type verification with invalid parameters" in {
    val output = processFile("controls.typeVerificationParameterUsable.invalidParameterType", configuration)
    output should beKo(
      TypeVerificationParameterUsableControl.errorUnknownType("UnknownNumber", invalidParameterLocation(4, 28, 64)),
      TypeVerificationParameterUsableControl.errorUnknownType("UnknownNumber", invalidParameterLocation(13, 28, 64))
    )
  }
}

object TypeVerificationParameterUsableControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(TypeVerificationParameterUsableControl.name)

  val invalidParameterLocation = LocationPath.control(TypeVerificationParameterUsableControl.name, "invalidParameterType")
}