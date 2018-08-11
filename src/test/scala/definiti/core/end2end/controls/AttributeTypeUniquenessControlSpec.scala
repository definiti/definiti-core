package definiti.core.end2end.controls

import definiti.common.ast.Root
import definiti.common.program.Ko
import definiti.common.tests.{ConfigurationMock, LocationPath}
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.AttributeTypeUniquenessControl

class AttributeTypeUniquenessControlSpec extends EndToEndSpec {

  import AttributeTypeUniquenessControlSpec._

  "Project.generatePublicAST" should "validate a defined type with several attribute types with different names" in {
    val output = processFile("controls.attributeTypeUniqueness.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate a defined type with two attribute types with the same name" in {
    val output = processFile("controls.attributeTypeUniqueness.duplicated", configuration)
    output should beResult(Ko[Root](
      AttributeTypeUniquenessControl.errorSameName(
        "Location",
        duplicatedLocation(3, 3, 29),
        duplicatedLocation(4, 3, 34)
      )
    ))
  }

  it should "validate two defined types with one attribute type each having the same name" in {
    val output = processFile("controls.attributeTypeUniqueness.distinctDefinedTypes", configuration)
    output shouldBe ok[Root]
  }
}

object AttributeTypeUniquenessControlSpec {
  val configuration = ConfigurationMock().withOnlyControls(AttributeTypeUniquenessControl)

  val duplicatedLocation = LocationPath.control(AttributeTypeUniquenessControl, "duplicated")
}

