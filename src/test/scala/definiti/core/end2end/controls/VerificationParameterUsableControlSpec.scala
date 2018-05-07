package definiti.core.end2end.controls

import definiti.common.ast.{Root, TypeReference}
import definiti.common.program.Ko
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.VerificationParameterUsableControl

class VerificationParameterUsableControlSpec extends EndToEndSpec {

  import VerificationParameterUsableControlSpec._

  "Project.generatePublicAST" should "validate a verification with valid parameter types and its use in function" in {
    val output = processFile("controls.verificationParameterUsable.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate a verification with a valid defined type" in {
    val output = processFile("controls.verificationParameterUsable.package", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an undefined parameter type" in {
    val output = processFile("controls.verificationParameterUsable.invalidParameterType", configuration)
    output should beResult(Ko[Root](
      VerificationParameterUsableControl.errorUnauthorizedType("LengthOf.length", TypeReference("UnknownNumber"), invalidParameterTypeLocation(1, 23, 44))
    ))
  }

  it should "invalidate non-atomic types (boolean, number, string) for parameters" in {
    val output = processFile("controls.verificationParameterUsable.unauthorizedParameterType", configuration)
    output should beResult(Ko[Root](
      VerificationParameterUsableControl.errorUnauthorizedType("LengthOf.anyAlias", TypeReference("AnyAlias"), unauthorizedParameterTypeLocation(7, 23, 41)),
      VerificationParameterUsableControl.errorUnauthorizedType("LengthOf.anyType", TypeReference("AnyType"), unauthorizedParameterTypeLocation(7, 43, 59))
    ))
  }
}

object VerificationParameterUsableControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(VerificationParameterUsableControl.name)

  val invalidParameterTypeLocation = LocationPath.control(VerificationParameterUsableControl.name, "invalidParameterType")
  val unauthorizedParameterTypeLocation = LocationPath.control(VerificationParameterUsableControl.name, "unauthorizedParameterType")
}
