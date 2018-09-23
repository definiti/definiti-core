package definiti.core.end2end.aliasType.closed

import definiti.common.ast.Root
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec

class TransparentTypeSpec extends EndToEndSpec {
  "Project.generatePublicAST" should "refuse to call a method from a transparent type" in {
    val output = processFile("aliasTypes.transparent.methodCall")
    output shouldBe ok[Root]
  }

  it should "refuse to call an attribute from a transparent type" in {
    val output = processFile("aliasTypes.transparent.attributeCall")
    output shouldBe ok[Root]
  }

  it should "accept a comparison != between the same transparent type" in {
    val output = processFile("aliasTypes.transparent.comparisonWithSameType")
    output shouldBe ok[Root]
  }

  it should "accept a comparison == between a transparent type and its alias" in {
    val output = processFile("aliasTypes.transparent.comparisonWithAlias")
    output shouldBe ok[Root]
  }

  it should "accept a comparison == between two transparent types with the same alias" in {
    val output = processFile("aliasTypes.transparent.comparisonWithSameAlias")
    output shouldBe ok[Root]
  }

  it should "accept an inequality between two transparent Number" in {
    val output = processFile("aliasTypes.transparent.inequalityWithNumberAlias")
    output shouldBe ok[Root]
  }

  it should "accept a function call when providing the exact same transparent type" in {
    val output = processFile("aliasTypes.transparent.parameterSameType")
    output shouldBe ok[Root]
  }

  it should "accept a function call when providing the alias type for a transparent type" in {
    val output = processFile("aliasTypes.transparent.parameterAlias")
    output shouldBe ok[Root]
  }

  it should "accept a function call when providing a transparent type for the same alias than the transparent type" in {
    val output = processFile("aliasTypes.transparent.parameterSameAlias")
    output shouldBe ok[Root]
  }
}
