package definiti.core.end2end

import definiti.common.ast._
import definiti.common.program.{Ko, Ok}
import definiti.common.tests.LocationPath
import definiti.common.utils.ASTUtils._
import definiti.common.validation.AlertLocation
import definiti.core.ProgramResultMatchers._

class VerificationSpec extends EndToEndSpec {
  import VerificationSpec._

  "Project.generatePublicAST" should "generate the AST for a valid verification with a generic" in {
    val expected = Ok(validNonEmptyList)
    val output = processFile("verification.NonEmptyList")
    output should beResult[Root](expected)
  }

  it should "generate the AST for a valid verification without a type name" in {
    val expected = Ok(validNonEmptyString)
    val output = processFile("verification.NonEmptyString")
    output should beResult[Root](expected)
  }

  it should "refuse to generate the AST for a verification without a type name and an invalid parameter name" in {
    val expected = Ko[Root](nonEmptyUnknown)
    val output = processFile("verification.NonEmptyUnknown")
    output should beResult(expected)
  }
}

object VerificationSpec {
  val nonEmptyListSrc = "src/test/resources/samples/verification/NonEmptyList.def"
  val nonEmptyListLocation = LocationPath(nonEmptyListSrc)
  val validNonEmptyList = root(
    Verification(
      name = "NonEmptyList",
      fullName = "NonEmptyList",
      parameters = Seq.empty,
      message = LiteralMessage("The list should not be empty", nonEmptyListLocation(2, 3, 33)),
      function = DefinedFunction(
        parameters = Seq(ParameterDefinition("list", TypeReference("List", Seq(TypeReference("A"))), nonEmptyListLocation(3, 7, 20))),
        body = MethodCall(
          expression = Reference("list", TypeReference("List", Seq(TypeReference("A"))), nonEmptyListLocation(4, 5, 9)),
          method = "nonEmpty",
          parameters = Seq.empty,
          generics = Seq.empty,
          returnType = TypeReference("Boolean"),
          location = nonEmptyListLocation(4, 5, 20)
        ),
        genericTypes = Seq("A"),
        location = nonEmptyListLocation(3, 3, 5, 4)
      ),
      comment = None,
      location = nonEmptyListLocation(1, 1, 6, 2)
    )
  )

  val nonEmptyStringSrc = "src/test/resources/samples/verification/NonEmptyString.def"
  val nonEmptyStringLocation = LocationPath(nonEmptyStringSrc)
  val validNonEmptyString = root(
    Verification(
      name = "NonEmptyString",
      fullName = "NonEmptyString",
      parameters = Seq.empty,
      message = LiteralMessage("The string should not be empty", nonEmptyStringLocation(2, 3, 35)),
      function = DefinedFunction(
        parameters = Seq(ParameterDefinition("string", TypeReference("String"), nonEmptyStringLocation(3, 4, 10))),
        body = MethodCall(
          expression = Reference("string", TypeReference("String"), nonEmptyStringLocation(4, 5, 11)),
          method = "nonEmpty",
          parameters = Seq.empty,
          generics = Seq.empty,
          returnType = TypeReference("Boolean"),
          location = nonEmptyStringLocation(4, 5, 22)
        ),
        genericTypes = Seq.empty,
        location = nonEmptyStringLocation(3, 3, 5, 4)
      ),
      comment = None,
      location = nonEmptyStringLocation(1, 1, 6, 2)
    )
  )

  val nonEmptyUnknownSrc = "src/test/resources/samples/verification/NonEmptyUnknown.def"
  val nonEmptyUnknown = Seq(
    AlertLocation(
      "Class Unknown not found when trying to determine the type of the expression",
      Location(nonEmptyUnknownSrc, 4, 5, 4, 12)
    )
  )
}