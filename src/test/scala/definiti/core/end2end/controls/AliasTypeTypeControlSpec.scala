package definiti.core.end2end.controls

import definiti.core.Ko
import definiti.core.ProgramResultMatchers._
import definiti.core.ast.Root
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.AliasTypeTypeControl

class AliasTypeTypeControlSpec extends EndToEndSpec {
  import AliasTypeTypeControlSpec._

  "Project.generatePublicAST" should "validate an alias type with a valid native type" in {
    val output = processFile("controls.aliasTypeType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate an alias type with a valid defined type" in {
    val output = processFile("controls.aliasTypeType.definedType", configuration)
    output shouldBe ok[Root]
  }

  it should "validate an alias type with a valid alias type" in {
    val output = processFile("controls.aliasTypeType.aliasType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an alias type with an unknown type" in {
    val output = processFile("controls.aliasTypeType.unknownType", configuration)
    output should beResult(Ko[Root](
      AliasTypeTypeControl.errorUnknownType("Unknown", unknownTypeLocation(1, 1, 28))
    ))
  }
}

object AliasTypeTypeControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(AliasTypeTypeControl.name)

  val unknownTypeLocation = LocationPath.control(AliasTypeTypeControl.name, "unknownType")
}