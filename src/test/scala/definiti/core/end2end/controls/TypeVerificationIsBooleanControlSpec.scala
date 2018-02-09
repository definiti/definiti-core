package definiti.core.end2end.controls

import definiti.core.ProgramResultMatchers._
import definiti.core._
import definiti.core.ast.{LiteralMessage, Root}
import definiti.core.end2end.EndToEndSpec
import definiti.core.validation.controls.TypeVerificationIsBooleanControl

class TypeVerificationIsBooleanControlSpec extends EndToEndSpec {
  import TypeVerificationIsBooleanControlSpec._

  "Project.generatePublicAST" should "validate a type verification returning boolean" in {
    val output = processFile("controls.typeVerificationIsBoolean.nominal", configuration)
    output shouldBe ok[Root]
  }

  it should "invalidate an alias type verification returning number" in {
    val expected = Ko[Root](
      TypeVerificationIsBooleanControl.errorNotBoolean(
        typeName = "InvalidNumber",
        message = LiteralMessage("This verification is invalid because returns a number", numberAliasLocation(3, 5, 60)),
        returnType = Constants.number,
        location = numberAliasLocation(4, 5, 6, 6)
      )
    )
    val output = processFile("controls.typeVerificationIsBoolean.numberAlias", configuration)
    output should beResult(expected)
  }

  it should "invalidate a defined type verification returning number" in {
    val expected = Ko[Root](
      TypeVerificationIsBooleanControl.errorNotBoolean(
        typeName = "InvalidNumber",
        message = LiteralMessage("This verification is invalid because returns a number", numberDefinedLocation(5, 5, 60)),
        returnType = Constants.number,
        location = numberDefinedLocation(6, 5, 8, 6)
      )
    )
    val output = processFile("controls.typeVerificationIsBoolean.numberDefined", configuration)
    output should beResult(expected)
  }

  it should "invalidate an alias type verification when condition does not return a boolean on each branch" in {
    val expected = Ko[Root](
      TypeVerificationIsBooleanControl.errorNotBoolean(
        typeName = "InvalidCondition",
        message = LiteralMessage("This verification is invalid because does not return the same type on if and else so not a boolean", conditionAliasLocation(3, 5, 105)),
        returnType = Constants.unit,
        location = conditionAliasLocation(4, 5, 10, 6)
      )
    )
    val output = processFile("controls.typeVerificationIsBoolean.conditionAlias", configuration)
    output should beResult(expected)
  }

  it should "invalidate a defined type verification when condition does not return a boolean on each branch" in {
    val expected = Ko[Root](
      TypeVerificationIsBooleanControl.errorNotBoolean(
        typeName = "InvalidCondition",
        message = LiteralMessage("This verification is invalid because does not return the same type on if and else so not a boolean", conditionDefinedLocation(5, 5, 105)),
        returnType = Constants.unit,
        location = conditionDefinedLocation(6, 5, 12, 6)
      )
    )
    val output = processFile("controls.typeVerificationIsBoolean.conditionDefined", configuration)
    output should beResult(expected)
  }
}

object TypeVerificationIsBooleanControlSpec {
  import EndToEndSpec._

  val configuration = configurationForceControls(TypeVerificationIsBooleanControl.name)

  val numberAliasLocation = LocationPath.control(TypeVerificationIsBooleanControl.name, "numberAlias")
  val numberDefinedLocation = LocationPath.control(TypeVerificationIsBooleanControl.name, "numberDefined")
  val conditionAliasLocation = LocationPath.control(TypeVerificationIsBooleanControl.name, "conditionAlias")
  val conditionDefinedLocation = LocationPath.control(TypeVerificationIsBooleanControl.name, "conditionDefined")
}