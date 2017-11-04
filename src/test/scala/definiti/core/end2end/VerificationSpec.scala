package definiti.core.end2end

import definiti.core.ValidValue
import definiti.core.ValidationMatchers.beValidated
import definiti.core.ast._

class VerificationSpec extends EndToEndSpec {
  import VerificationSpec._

  "Project.generatePublicAST" should "generate the AST for a valid verification with a generic" in {
    val expected = ValidValue(validNonEmptyList)
    val output = processFile("verification.NonEmptyList")
    output should beValidated[Root](expected)
  }
}

object VerificationSpec {
  import EndToEndSpec._

  val nonEmptyListSrc = "src/test/resources/samples/verification/NonEmptyList.def"
  val nonEmptyListLocation = LocationPath(nonEmptyListSrc)
  val validNonEmptyList = Root(Seq(
    Verification(
      name = "NonEmptyList",
      message = "The list should not be empty",
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
  ))
}