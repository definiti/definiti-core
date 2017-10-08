package definiti.core.end2end

import definiti.core.ast._
import definiti.core._

class NominalSpec extends EndToEndSpec {
  import NominalSpec._
  import ValidationMatchers._

  "Project.generatePublicAST" should "generate a valid AST for a valid defined type" in {
    val expected = ValidValue(definedType)
    val output = processFile("nominal.definedType")
    output should beValidated[Root](expected)
  }

  it should "generate a valid AST for a valid alias type" in {
    val expected = ValidValue(aliasType)
    val output = processFile("nominal.aliasType")
    output should beValidated[Root](expected)
  }

  it should "generate a valid AST for a valid verification" in {
    val expected = ValidValue(verification)
    val output = processFile("nominal.verification")
    output should beValidated[Root](expected)
  }

  it should "generate a valid AST for a valid named function" in {
    val expected = ValidValue(namedFunction)
    val output = processFile("nominal.namedFunction")
    output should beValidated[Root](expected)
  }

  it should "generate a valid AST for a valid extended context" in {
    val expected = ValidValue(extendedContext)
    val output = processFile("nominal.extendedContext")
    output should beValidated[Root](expected)
  }

  it should "generate a valid AST for a valid alias type in a package" in {
    val expected = ValidValue(packageAliasType)
    val output = processFile("nominal.package")
    output should beValidated[Root](expected)
  }
}

object NominalSpec {
  val definedType: Root = Root(
    elements = Seq(DefinedType(
      name = "MyType",
      genericTypes = Seq.empty,
      attributes = Seq(
        AttributeDefinition(
          name = "myAttribute",
          typeReference = TypeReference("String"),
          comment = None,
          verifications = Seq.empty,
          range = Range(2, 3, 2, 22)
        )
      ),
      verifications = Seq.empty,
      inherited = Seq.empty,
      comment = None,
      range = Range(1, 1, 3, 2)
    ))
  )

  val aliasType: Root = Root(
    elements = Seq(AliasType(
      name = "AliasString",
      genericTypes = Seq.empty,
      alias = TypeReference("String"),
      inherited = Seq.empty,
      comment = None,
      range = Range(1, 1, 1, 26)
    ))
  )

  val verification: Root = Root(
    elements = Seq(Verification(
      name = "AlwaysTrue",
      message = "Never fail",
      function = DefinedFunction(
        parameters = Seq(ParameterDefinition(
          name = "x",
          typeReference = TypeReference("String"),
          range = Range(3, 4, 3, 13)
        )),
        body = BooleanValue(value = true, TypeReference("Boolean"), Range(4, 5, 4, 9)),
        genericTypes = Seq.empty,
        range = Range(3, 3, 5, 4)
      ),
      comment = None,
      range = Range(1, 1, 6, 2)
    ))
  )

  val namedFunction: Root = Root(
    elements = Seq(NamedFunction(
      name = "alwaysFalse",
      genericTypes = Seq.empty,
      parameters = Seq.empty,
      returnType = TypeReference("Boolean"),
      body = BooleanValue(value = false, TypeReference("Boolean"), Range(2, 3, 2, 8)),
      range = Range(1, 1, 3, 2)
    ))
  )

  val extendedContext: Root = Root(
    elements = Seq(ExtendedContext(
      name = "stringContext",
      content = "Something here",
      range = Range(2, 3, 2, 17)
    ))
  )

  val packageAliasType: Root = Root(
    elements = Seq(Namespace(
      name = "tst",
      elements = Seq(AliasType(
        name = "AliasString",
        genericTypes = Seq.empty,
        alias = TypeReference("String"),
        inherited = Seq.empty,
        comment = None,
        range = Range(3, 1, 3, 26)
      ))
    ))
  )
}
