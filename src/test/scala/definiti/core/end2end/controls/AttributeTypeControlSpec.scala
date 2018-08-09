package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
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
      AttributeTypeControl.errorUnknownType("Unknown", unknownTypeLocation(2, 3, 23))
    ))
  }

  it should "accept a valid reference to an attribute type" in {
    val output = processFile("controls.attributeType.validAttributeTypeReference", configuration)
    output shouldBe ok[Root]
  }

  it should "accept a valid reference to an attribute type in the same type" in {
    val output = processFile("controls.attributeType.validInternalAttributeTypeReference", configuration)
    output shouldBe ok[Root]
  }

  it should "refuse an invalid reference to an attribute type" in {
    val output = processFile("controls.attributeType.invalidAttributeTypeReference", configuration)
    output should beResult(Ko[Root](
      AttributeTypeControl.errorUnknownType("Order.Unknown", invalidAttributeTypeReferenceLocation(6, 3, 23))
    ))
  }
}

object AttributeTypeControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(AttributeTypeControl)

  val unknownTypeLocation = LocationPath.control(AttributeTypeControl, "unknownType")
  val invalidAttributeTypeReferenceLocation = LocationPath.control(AttributeTypeControl, "invalidAttributeTypeReference")
}