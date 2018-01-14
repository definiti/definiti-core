package definiti.core.end2end

import definiti.core.ast.{Location, Root}
import definiti.core.{Invalid, ValidationMatchers}

class EnumSpec extends EndToEndSpec {
  import ValidationMatchers._

  "Project.generatePublicAST" should "validate the use in identity function" in {
    val output = processFile("enum.identity")
    output should be(valid)
  }

  it should "accept reading value directly from enum" in {
    val output = processFile("enum.defGivingValue")
    output should be(valid)
  }

  it should "refuse reading unknown value directly from enum" in {
    val output = processFile("enum.defGivingInvalidValue")
    output should beValidated[Root](Invalid(
      message = "Unknown attribute MyEnum.invalid",
      location = Location("src/test/resources/samples/enum/defGivingInvalidValue.def", 7, 3, 7, 17)
    ))
  }
}