package definiti.core.end2end.controls

import definiti.core.Ko
import definiti.core.ProgramResultMatchers._
import definiti.core.ast._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.TypeParameterUsableControl

class TypeParameterUsableControlSpec extends EndToEndSpec {

  import TypeParameterUsableControlSpec._

  "Project.generatePublicAST" should "validate alias and defined types with valid parameter types and its use in inline verifications" in {
    val output = processFile("controls.typeParameterUsable.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate alias and defined types with valid parameter in a package" in {
    val output = processFile("controls.typeParameterUsable.package", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an undefined parameter type" in {
    val output = processFile("controls.typeParameterUsable.invalidParameterType", configuration)
    output should beResult(Ko[Root](
      TypeParameterUsableControl.errorUnauthorizedType("Contact.numberOfPhones", TypeReference("UnknownNumber"), invalidParameterTypeLocation(1, 14, 43)),
      TypeParameterUsableControl.errorUnauthorizedType("StringOfLength.length", TypeReference("UnknownNumber"), invalidParameterTypeLocation(16, 21, 42))
    ))
  }

  it should "invalidate non-atomic types (boolean, number, string) for parameters" in {
    val output = processFile("controls.typeParameterUsable.unauthorizedParameterType", configuration)
    output should beResult(Ko[Root](
      TypeParameterUsableControl.errorUnauthorizedType("Contact.numberOfPhones", TypeReference("AnotherNumber"), unauthorizedParameterTypeLocation(3, 14, 43)),
      TypeParameterUsableControl.errorUnauthorizedType("StringOfLength.length", TypeReference("AnotherNumber"), unauthorizedParameterTypeLocation(18, 21, 42))
    ))
  }
}

object TypeParameterUsableControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(TypeParameterUsableControl.name)

  val invalidParameterTypeLocation = LocationPath.control(TypeParameterUsableControl.name, "invalidParameterType")
  val unauthorizedParameterTypeLocation = LocationPath.control(TypeParameterUsableControl.name, "unauthorizedParameterType")
}
