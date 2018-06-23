package definiti.core.end2end

import definiti.common.ast._
import definiti.common.program.{Ko, Ok}
import definiti.common.tests.LocationPath
import definiti.common.utils.ASTUtils._
import definiti.core.ProgramResultMatchers._
import definiti.core.validation.controls.{AttributeTypeControl, TypeDeclarationParametersControl}

class DefinedTypeSpec extends EndToEndSpec {
  import DefinedTypeSpec._

  "Project.generatePublicAST" should "generate the AST with a defined type with implicit attribute types" in {
    val expected = Ok[Root](implicitAttributeType)
    val output = processFile("definedType.implicit-attribute-type")
    output should beResult(expected)
  }

  it should "invalid the AST when an implicit type of an attribute does not exist" in {
    val expected = Ko[Root](invalidImplicitAttributeTypeErrors)
    val output = processFile("definedType.invalid-implicit-attribute-type")
    output should beResult(expected)
  }
}

object DefinedTypeSpec {
  val implicitAttributeTypeLocation = LocationPath("src/test/resources/samples/definedType/implicit-attribute-type.def")
  val implicitAttributeType = root(
    DefinedType(
      name = "Person",
      fullName = "Person",
      genericTypes = Seq.empty,
      parameters = Seq.empty,
      attributes = Seq(
        AttributeDefinition(
          name = "email",
          typeDeclaration = TypeDeclaration("Email", Seq.empty, Seq.empty, implicitAttributeTypeLocation(2, 3, 8)),
          comment = None,
          verifications = Seq(
            VerificationReference(
              verificationName = "IsNonEmptyEmail",
              parameters = Seq.empty,
              location = implicitAttributeTypeLocation(2, 9, 34)
            )
          ),
          location = implicitAttributeTypeLocation(2, 3, 34)
        ),
        AttributeDefinition(
          name = "address",
          typeDeclaration = TypeDeclaration("Address", Seq.empty, Seq.empty, implicitAttributeTypeLocation(3, 3, 10)),
          comment = None,
          verifications = Seq.empty,
          location = implicitAttributeTypeLocation(3, 3, 10)
        )
      ),
      verifications = Seq.empty,
      inherited = Seq.empty,
      comment = None,
      location = implicitAttributeTypeLocation(1, 1, 4, 2)
    ),
    AliasType(
      name = "Email",
      fullName = "Email",
      genericTypes = Seq.empty,
      parameters = Seq.empty,
      alias = TypeDeclaration("String", Seq.empty, Seq.empty, implicitAttributeTypeLocation(6, 14, 20)),
      inherited = Seq.empty,
      verifications = Seq.empty,
      comment = None,
      location = implicitAttributeTypeLocation(6, 1, 20)
    ),
    DefinedType(
      name = "Address",
      fullName = "Address",
      genericTypes = Seq.empty,
      parameters = Seq.empty,
      attributes = Seq(
        AttributeDefinition(
          name = "city",
          typeDeclaration = TypeDeclaration("String", Seq.empty, Seq.empty, implicitAttributeTypeLocation(9, 9, 15)),
          comment = None,
          verifications = Seq.empty,
          location = implicitAttributeTypeLocation(9, 3, 15)
        ),
        AttributeDefinition(
          name = "country",
          typeDeclaration = TypeDeclaration("String", Seq.empty, Seq.empty, implicitAttributeTypeLocation(10, 12, 18)),
          comment = None,
          verifications = Seq.empty,
          location = implicitAttributeTypeLocation(10, 3, 18)
        )
      ),
      verifications = Seq.empty,
      inherited = Seq.empty,
      comment = None,
      location = implicitAttributeTypeLocation(8, 1, 11, 2)
    ),
    Verification(
      name = "IsNonEmptyEmail",
      fullName = "IsNonEmptyEmail",
      parameters = Seq.empty,
      message = LiteralMessage("Please give a non empty email", implicitAttributeTypeLocation(14, 3, 34)),
      function = DefinedFunction(
        parameters = Seq(ParameterDefinition("email", TypeReference("Email"), implicitAttributeTypeLocation(15, 4, 16))),
        body = MethodCall(
          expression = Reference("email", TypeReference("Email"), implicitAttributeTypeLocation(16, 5, 10)),
          method = "nonEmpty",
          parameters = Seq.empty,
          generics = Seq.empty,
          returnType = TypeReference("Boolean"),
          location = implicitAttributeTypeLocation(16, 5, 21)
        ),
        genericTypes = Seq.empty,
        location = implicitAttributeTypeLocation(15, 3, 17, 4)
      ),
      comment = None,
      location = implicitAttributeTypeLocation(13, 1, 18, 2)
    )
  )

  val invalidImplicitAttributeTypeErrorsLocation = LocationPath("src/test/resources/samples/definedType/invalid-implicit-attribute-type.def")
  val invalidImplicitAttributeTypeErrors = Seq(
    AttributeTypeControl.errorUnknownType("Email", invalidImplicitAttributeTypeErrorsLocation(2, 3, 8)),
    AttributeTypeControl.errorUnknownType("Address", invalidImplicitAttributeTypeErrorsLocation(3, 3, 10)),
    TypeDeclarationParametersControl.errorUnknownType("Email", invalidImplicitAttributeTypeErrorsLocation(2, 3, 8)),
    TypeDeclarationParametersControl.errorUnknownType("Address", invalidImplicitAttributeTypeErrorsLocation(3, 3, 10))
  )
}