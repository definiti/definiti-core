package definiti.core.end2end

import definiti.common.ast._
import definiti.common.program.Ok
import definiti.core.ProgramResultMatchers._

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

  it should "generate a valid AST for a valid package with sub packages" in {
    val expected = Ok(packages)
    val output = processDirectory("nominal.packages")
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
      parameters = Seq.empty,
      attributes = Seq(
        AttributeDefinition(
          name = "myAttribute",
          typeDeclaration = TypeDeclaration("String", Seq.empty, Seq.empty, Location(definedTypeSrc, 2, 16, 2, 22)),
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
      parameters = Seq.empty,
      alias = TypeDeclaration("String", Seq.empty, Seq.empty, Location(aliasTypeSrc, 1, 20, 1, 26)),
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
      parameters = Seq.empty,
      message = LiteralMessage("Never fail", Location(verificationSrc, 2, 3, 2, 15)),
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
        parameters = Seq.empty,
        alias = TypeDeclaration("String", Seq.empty, Seq.empty, Location(packageAliasTypeSrc, 3, 20, 3, 26)),
        verifications = Seq.empty,
        inherited = Seq.empty,
        comment = None,
        location = Location(packageAliasTypeSrc, 3, 1, 3, 26)
      ))
    ))
  )

  val packagesPageSrc = "src/test/resources/samples/nominal/packages/page/Page.def"
  val packagesStringSrc = "src/test/resources/samples/nominal/packages/verifications/string.def"
  val packages: Root = Root(
    elements = Seq(Namespace(
      name = "packages",
      fullName = "packages",
      elements = Seq(Namespace(
        name = "common",
        fullName = "packages.common",
        elements = Seq(
          Namespace(
            name = "page",
            fullName = "packages.common.page",
            elements = Seq(DefinedType(
              name = "Page",
              fullName = "packages.common.page.Page",
              genericTypes = Seq.empty,
              parameters = Seq.empty,
              attributes = Seq(
                AttributeDefinition(
                  name = "title",
                  typeDeclaration = TypeDeclaration(
                    typeName = "String",
                    genericTypes = Seq.empty,
                    parameters = Seq.empty,
                    location = Location(packagesPageSrc, 6, 10, 6, 16)
                  ),
                  comment = None,
                  verifications = Seq(
                    VerificationReference(
                      verificationName = "packages.common.verifications.IsNonBlank",
                      parameters = Seq(QuotedStringValue(
                        value = "Please give a title",
                        returnType = TypeReference("String"),
                        location = Location(packagesPageSrc, 6, 38, 6, 59)
                      )),
                      location = Location(packagesPageSrc, 6, 17, 6, 60)
                    )
                  ),
                  location = Location(packagesPageSrc, 6, 3, 6, 60)
                )
              ),
              verifications = Seq.empty,
              inherited = Seq.empty,
              comment = None,
              location = Location(packagesPageSrc, 5, 1, 7, 2)
            ))
          ),
          Namespace(
            name = "verifications",
            fullName = "packages.common.verifications",
            elements = Seq(Verification(
              name = "IsNonBlank",
              fullName = "packages.common.verifications.IsNonBlank",
              parameters = Seq.empty,
              message = LiteralMessage("The field is required", Location(packagesStringSrc, 4, 3, 4, 26)),
              function = DefinedFunction(
                parameters = Seq(ParameterDefinition("string", TypeReference("String"), Location(packagesStringSrc, 5, 4, 5, 18))),
                body = MethodCall(
                  expression = MethodCall(
                    expression = Reference(
                      name = "string",
                      returnType = TypeReference("String"),
                      location = Location(packagesStringSrc, 6, 5, 6, 11)
                    ),
                    method = "trim",
                    parameters = Seq.empty,
                    generics = Seq.empty,
                    returnType = TypeReference("String"),
                    location = Location(packagesStringSrc, 6, 5, 6, 18)
                  ),
                  method = "nonEmpty",
                  parameters = Seq.empty,
                  generics = Seq.empty,
                  returnType = TypeReference("Boolean"),
                  location = Location(packagesStringSrc, 6, 5, 6, 29)
                ),
                genericTypes = Seq.empty,
                location = Location(packagesStringSrc, 5, 3, 7, 4)
              ),
              comment = None,
              location = Location(packagesStringSrc, 3, 1, 8, 2)
            ))
          )
        )
      ))
    ))
  )
}
