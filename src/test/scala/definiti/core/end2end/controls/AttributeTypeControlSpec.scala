package definiti.core.end2end.controls

import definiti.core.Ko
import definiti.core.ProgramResultMatchers._
import definiti.core.ast.Root
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.AttributeTypeControl

class AttributeTypeControlSpec extends EndToEndSpec {
  import AttributeTypeControlSpec._

  "Project.generatePublicAST" should "validate an attribute with a valid native type" in {
    val output = processFile("controls.attributeType.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "validate an attribute with a valid defined type" in {
    val output = processFile("controls.attributeType.definedType", configuration)
    output shouldBe ok[Root]
  }

  it should "validate an attribute with a valid alias type" in {
    val output = processFile("controls.attributeType.aliasType", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an attribute with an unknown type" in {
    val output = processFile("controls.attributeType.unknownType", configuration)
    output should beResult(Ko[Root](
      AttributeTypeControl.errorUnknownType("Unknown", "MyType.myAttribute", unknownTypeLocation(2, 3, 23))
    ))
  }
}

object AttributeTypeControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(AttributeTypeControl.name)

  val unknownTypeLocation = LocationPath.control(AttributeTypeControl.name, "unknownType")
}