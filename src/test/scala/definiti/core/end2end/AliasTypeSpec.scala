package definiti.core.end2end

import definiti.core.ValidValue
import definiti.core.ValidationMatchers._
import definiti.core.ast._

class AliasTypeSpec extends EndToEndSpec {
  import AliasTypeSpec._

  "Project.generatePublicAST" should "generate the AST with an alias containing generics" in {
    val output = processFile("aliasTypes.ListAlias")
    output shouldBe valid[Root]
  }

  it should "generate the AST with an inline verification" in {
    val expected = ValidValue(inlineVerification)
    val output = processFile("aliasTypes.inline-verification")
    output should beValidated[Root](expected)
  }

  it should "invalid the AST when the inline verification is invalid" in {
    val output = processFile("aliasTypes.invalid-inline-verification")
    output shouldBe invalid
  }
}

object AliasTypeSpec {
  val inlineVerificationFile = "src/test/resources/samples/aliasTypes/inline-verification.def"
  val inlineVerification = Root(Seq(
    AliasType(
      name = "ListAlias",
      genericTypes = Seq("A"),
      alias = TypeReference("List", Seq(TypeReference("A"))),
      inherited = Seq.empty,
      verifications = Seq(TypeVerification(
        message = "The list should not be empty",
        function = DefinedFunction(
          parameters = Seq(ParameterDefinition(
            name = "list",
            typeReference = TypeReference("ListAlias", Seq(TypeReference("A"))),
            location = Location(inlineVerificationFile, 4, 6, 4, 10)
          )),
          body = MethodCall(
            expression = Reference(
              name = "list",
              returnType = TypeReference("ListAlias", Seq(TypeReference("A"))),
              location = Location(inlineVerificationFile, 5, 7, 5, 11)
            ),
            method = "nonEmpty",
            parameters = Seq.empty,
            generics = Seq.empty,
            returnType = TypeReference("Boolean"),
            location = Location(inlineVerificationFile, 5, 7, 5, 22)
          ),
          genericTypes = Seq.empty,
          location = Location(inlineVerificationFile, 4, 5, 6, 6)
        ),
        location = Location(inlineVerificationFile, 2, 3, 7, 4)
      )),
      comment = None,
      location = Location(inlineVerificationFile, 1, 1, 8, 2)
    )
  ))
}