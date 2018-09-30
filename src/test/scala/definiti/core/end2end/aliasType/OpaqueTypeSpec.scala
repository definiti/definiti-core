package definiti.core.end2end.aliasType

import definiti.common.ast.Root
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec

class OpaqueTypeSpec extends EndToEndSpec {
  "Project.generatePublicAST" should "refuse to call a method from an opaque type" in {
    val output = processFile("aliasTypes.opaque.methodCallInvalid")
    output shouldBe ko[Root]
  }

  it should "refuse to call an attribute from an opaque type" in {
    val output = processFile("aliasTypes.opaque.attributeCallInvalid")
    output shouldBe ko[Root]
  }

  it should "accept a comparison == between the same opaque type" in {
    val output = processFile("aliasTypes.opaque.compareValidEquals")
    output shouldBe ok[Root]
  }

  it should "accept a comparison != between the same opaque type" in {
    val output = processFile("aliasTypes.opaque.compareValidDifferent")
    output shouldBe ok[Root]
  }

  it should "refuse a comparison != between an opaque type and its alias on left side" in {
    val output = processFile("aliasTypes.opaque.compareInvalidLeft")
    output shouldBe ko[Root]
  }

  it should "refuse a comparison != between an opaque type and its alias on right side" in {
    val output = processFile("aliasTypes.opaque.compareInvalidRight")
    output shouldBe ko[Root]
  }

  it should "accept a function call when providing the exact same opaque type" in {
    val output = processFile("aliasTypes.opaque.parameterTypeValid")
    output shouldBe ok[Root]
  }

  it should "refuse a function call when providing the alias of the opaque type" in {
    val output = processFile("aliasTypes.opaque.parameterTypeInvalid")
    output shouldBe ko[Root]
  }

  it should "accept a valid method" in {
    val output = processFile("aliasTypes.opaque.validMethod")
    output shouldBe ok[Root]
  }
}
