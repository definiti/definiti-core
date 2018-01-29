package definiti.core.end2end

import definiti.core.ast._
import definiti.core._
import ProgramResultMatchers._

class NominalSpec extends EndToEndSpec {
  import NominalSpec._

  "Project.generatePublicAST" should "generate a valid AST for a valid defined type" in {
    val expected = Ok(definedType)
    val output = processFile("nominal.definedType")
    output should beResult[Root](expected)
  }

  it should "generate a valid AST for a valid alias type" in {
    val expected = Ok(aliasType)
    val output = processFile("nominal.aliasType")
    output should beResult[Root](expected)
  }

  it should "generate a valid AST for a valid enum" in {
    val expected = Ok(enum)
    val output = processFile("nominal.enum")
    output should beResult[Root](expected)
  }

  it should "generate a valid AST for a valid verification" in {
    val expected = Ok(verification)
    val output = processFile("nominal.verification")
    output should beResult[Root](expected)
  }

  it should "generate a valid AST for a valid named function" in {
    val expected = Ok(namedFunction)
    val output = processFile("nominal.namedFunction")
    output should beResult[Root](expected)
  }

  it should "generate a valid AST for a valid extended context" in {
    val expected = Ok(extendedContext)
    val output = processFile("nominal.extendedContext")
    output should beResult[Root](expected)
  }

  it should "generate a valid AST for a valid alias type in a package" in {
    val expected = Ok(packageAliasType)
    val output = processFile("nominal.package")
    output should beResult[Root](expected)
  }
}

object NominalSpec {
  val definedTypeSrc = "src/test/resources/samples/nominal/definedType.def"
  val definedType: Root = Root(
    elements = Seq(DefinedType(
      name = "MyType",
      fullName = "MyType",
      genericTypes = Seq.empty,
      attributes = Seq(
        AttributeDefinition(
          name = "myAttribute",
          typeReference = TypeReference("String"),
          comment = None,
          verifications = Seq.empty,
          location = Location(definedTypeSrc, 2, 3, 2, 22)
        )
      ),
      verifications = Seq.empty,
      inherited = Seq.empty,
      comment = None,
      location = Location(definedTypeSrc, 1, 1, 3, 2)
    ))
  )

  val aliasTypeSrc = "src/test/resources/samples/nominal/aliasType.def"
  val aliasType: Root = Root(
    elements = Seq(AliasType(
      name = "AliasString",
      fullName = "AliasString",
      genericTypes = Seq.empty,
      alias = TypeReference("String"),
      verifications = Seq.empty,
      inherited = Seq.empty,
      comment = None,
      location = Location(aliasTypeSrc, 1, 1, 1, 26)
    ))
  )

  val enumSrc = "src/test/resources/samples/nominal/enum.def"
  val enum: Root = Root(
    elements = Seq(Enum(
      name = "MyEnum",
      fullName = "MyEnum",
      cases = Seq(
        EnumCase("First", None, Location(enumSrc, 2, 3, 2, 8)),
        EnumCase("Second", None, Location(enumSrc, 3, 3, 3, 9))
      ),
      comment = None,
      location = Location(enumSrc, 1, 1, 4, 2)
    ))
  )

  val verificationSrc = "src/test/resources/samples/nominal/verification.def"
  val verification: Root = Root(
    elements = Seq(Verification(
      name = "AlwaysTrue",
      fullName = "AlwaysTrue",
      message = "Never fail",
      function = DefinedFunction(
        parameters = Seq(ParameterDefinition(
          name = "x",
          typeReference = TypeReference("String"),
          location = Location(verificationSrc, 3, 4, 3, 13)
        )),
        body = BooleanValue(value = true, TypeReference("Boolean"), Location(verificationSrc, 4, 5, 4, 9)),
        genericTypes = Seq.empty,
        location = Location(verificationSrc, 3, 3, 5, 4)
      ),
      comment = None,
      location = Location(verificationSrc, 1, 1, 6, 2)
    ))
  )

  val namedFunctionSrc = "src/test/resources/samples/nominal/namedFunction.def"
  val namedFunction: Root = Root(
    elements = Seq(NamedFunction(
      name = "alwaysFalse",
      fullName = "alwaysFalse",
      genericTypes = Seq.empty,
      parameters = Seq.empty,
      returnType = TypeReference("Boolean"),
      body = BooleanValue(value = false, TypeReference("Boolean"), Location(namedFunctionSrc, 2, 3, 2, 8)),
      location = Location(namedFunctionSrc, 1, 1, 3, 2)
    ))
  )

  val extendedContextSrc = "src/test/resources/samples/nominal/extendedContext.def"
  val extendedContext: Root = Root(
    elements = Seq(ExtendedContext(
      name = "stringContext",
      content = "Something here",
      location = Location(extendedContextSrc, 2, 3, 2, 17)
    ))
  )

  val packageAliasTypeSrc = "src/test/resources/samples/nominal/package.def"
  val packageAliasType: Root = Root(
    elements = Seq(Namespace(
      name = "tst",
      fullName = "tst",
      elements = Seq(AliasType(
        name = "AliasString",
        fullName = "tst.AliasString",
        genericTypes = Seq.empty,
        alias = TypeReference("String"),
        verifications = Seq.empty,
        inherited = Seq.empty,
        comment = None,
        location = Location(packageAliasTypeSrc, 3, 1, 3, 26)
      ))
    ))
  )
}
