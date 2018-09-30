package definiti.core.end2end.controls.aliasType

import definiti.common.ast._
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.aliasType.MethodAcceptationControl

class MethodAcceptationControlSpec extends EndToEndSpec {
  import MethodAcceptationControlSpec._

  "Project.generatePublicAST" should "validate an opaque type with simple methods" in {
    val output = processFile("controls.aliasType.methodAcceptation.opaqueType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a closed type with methods" in {
    val output = processFile("controls.aliasType.methodAcceptation.closedType", configuration)
    output should beResult(Ko[Root](
      MethodAcceptationControl.errorMethodNotAccepted(AliasTypeKind.Closed, closedTypeLocation(2, 3, 4, 4)),
      MethodAcceptationControl.errorMethodNotAccepted(AliasTypeKind.Closed, closedTypeLocation(6, 3, 8, 4))
    ))
  }

  it should "invalidate a transparent type with methods" in {
    val output = processFile("controls.aliasType.methodAcceptation.transparentType", configuration)
    output should beResult(Ko[Root](
      MethodAcceptationControl.errorMethodNotAccepted(AliasTypeKind.Transparent, transparentTypeLocation(2, 3, 4, 4)),
      MethodAcceptationControl.errorMethodNotAccepted(AliasTypeKind.Transparent, transparentTypeLocation(6, 3, 8, 4))
    ))
  }
}

object MethodAcceptationControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(MethodAcceptationControl)

  val closedTypeLocation = LocationPath.controlAliasType(MethodAcceptationControl, "closedType")
  val transparentTypeLocation = LocationPath.controlAliasType(MethodAcceptationControl, "transparentType")
}