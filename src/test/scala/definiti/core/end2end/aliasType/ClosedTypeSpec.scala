package definiti.core.end2end.aliasType

import definiti.common.ast.Root
import definiti.core.ProgramResultMatchers._
import definiti.core.end2end.EndToEndSpec

class ClosedTypeSpec extends EndToEndSpec {
  "Project.generatePublicAST" should "refuse to call a method from a closed type" in {
    val output = processFile("aliasTypes.closed.methodCallInvalid")
    output shouldBe ko[Root]
  }

  it should "refuse to call an attribute from a closed type" in {
    val output = processFile("aliasTypes.closed.attributeCallInvalid")
    output shouldBe ko[Root]
  }

  it should "accept a comparison == between the same closed type" in {
    val output = processFile("aliasTypes.closed.compareValidEquals")
    output shouldBe ok[Root]
  }

  it should "accept a comparison != between the same closed type" in {
    val output = processFile("aliasTypes.closed.compareValidDifferent")
    output shouldBe ok[Root]
  }

  it should "refuse a comparison != between a closed type and its alias on left side" in {
    val output = processFile("aliasTypes.closed.compareInvalidLeft")
    output shouldBe ko[Root]
  }

  it should "refuse a comparison != between a closed type and its alias on right side" in {
    val output = processFile("aliasTypes.closed.compareInvalidRight")
    output shouldBe ko[Root]
  }

  it should "accept a function call when providing the exact same closed type" in {
    val output = processFile("aliasTypes.closed.parameterTypeValid")
    output shouldBe ok[Root]
  }

  it should "refuse a function call when providing the alias of the closed type" in {
    val output = processFile("aliasTypes.closed.parameterTypeInvalid")
    output shouldBe ko[Root]
  }

  it should "not accept a method for a closed type" in {
    val output = processFile("aliasTypes.closed.methodNotAccepted")
    output shouldBe ko[Root]
  }
}
