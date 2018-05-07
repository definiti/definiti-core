package definiti.core.end2end

import definiti.common.ast._
import definiti.common.program.Ko
import definiti.common.validation.AlertLocation
import definiti.core.ProgramResultMatchers._

class EnumSpec extends EndToEndSpec {
  "Project.generatePublicAST" should "validate the use in identity function" in {
    val output = processFile("enum.identity")
    output shouldBe ok[Root]
  }

  it should "accept reading value directly from enum" in {
    val output = processFile("enum.defGivingValue")
    output shouldBe ok[Root]
  }

  it should "refuse reading unknown value directly from enum" in {
    val output = processFile("enum.defGivingInvalidValue")
    output should beResult[Root](Ko(AlertLocation(
      message = "Unknown attribute MyEnum.invalid",
      location = Location("src/test/resources/samples/enum/defGivingInvalidValue.def", 7, 3, 7, 17)
    )))
  }
}