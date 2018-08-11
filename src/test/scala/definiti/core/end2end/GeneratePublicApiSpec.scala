package definiti.core.end2end

import definiti.common.ast._
import definiti.common.program.Ok
import definiti.core.ProgramResultMatchers._
import definiti.core._

class GeneratePublicApiSpec extends EndToEndSpec {
  import GeneratePublicApiSpec._

  "Project.generatePublicAST" should "generate the public API when the project is valid (sample: blog)" in {
    val expected = Ok(validBlogExpected)
    val output = processDirectory("blog")
    output should beResult(expected)
  }

  it should "return an error when the project is invalid (sample: invalid.blog)" in {
    val output = processDirectory("invalid.blog")
    output shouldBe ko
  }

  it should "return an error when the project is invalid (sample: invalid.blog2)" in {
    val output = processDirectory("invalid.blog2")
    output shouldBe ko
  }
}

object GeneratePublicApiSpec {
  val validBlogSrcTypes = "src/test/resources/samples/blog/types.def"
  val validBlogSrcVerifications = "src/test/resources/samples/blog/verifications.def"
  val validBlogExpected = Root(Seq(
    Namespace(
      name = "blog",
      fullName = "blog",
      elements = Seq(
        DefinedType(
          name = "Blog",
          fullName = "blog.Blog",
          genericTypes = Seq.empty,
          parameters = Seq.empty,
          attributes = Seq(
            AttributeDefinition(
              name = "title",
              typeDeclaration = TypeDeclaration("String", Seq.empty, Seq.empty, Location(validBlogSrcTypes, 7, 10, 7, 16)),
              comment = None,
              verifications = Seq(VerificationReference(
                "blog.NonBlank",
                Seq(QuotedStringValue("Please provide a title", Constants.string, Location(validBlogSrcTypes, 7, 36, 7, 60))),
                Location(validBlogSrcTypes, 7, 17, 7, 61)
              )),
              typeName = None,
              location = Location(validBlogSrcTypes, 7, 3, 7, 61)
            ),
            AttributeDefinition(
              name = "content",
              typeDeclaration = TypeDeclaration("String", Seq.empty, Seq.empty, Location(validBlogSrcTypes, 8, 12, 8, 18)),
              comment = None,
              verifications = Seq(VerificationReference("blog.NonBlank", Seq.empty, Location(validBlogSrcTypes, 8, 19, 8, 37))),
              typeName = None,
              location = Location(validBlogSrcTypes, 8, 3, 8, 37)
            ),
            AttributeDefinition(
              name = "tags",
              typeDeclaration = TypeDeclaration(
                typeName = "List",
                genericTypes = Seq(TypeDeclaration("String", Seq.empty, Seq.empty, Location(validBlogSrcTypes, 9, 14, 9, 20))),
                parameters = Seq.empty,
                location = Location(validBlogSrcTypes, 9, 9, 9, 21)
              ),
              comment = None,
              verifications = Seq.empty,
              typeName = None,
              location = Location(validBlogSrcTypes, 9, 3, 9, 21)
            )
          ),
          verifications = Seq(
            AtomicTypeVerification(
              message = LiteralMessage("No tag can be empty", Location(validBlogSrcTypes, 12, 5, 12, 26)),
              function = DefinedFunction(
                parameters = Seq(ParameterDefinition("blog", TypeReference("blog.Blog"), Location(validBlogSrcTypes, 13, 6, 13, 10))),
                body = Condition(
                  condition = MethodCall(
                    expression = AttributeCall(
                      expression = Reference("blog", TypeReference("blog.Blog"), Location(validBlogSrcTypes, 14, 11, 14, 15)),
                      attribute = "tags",
                      returnType = TypeReference("List", Seq(TypeReference("String"))),
                      location = Location(validBlogSrcTypes, 14, 11, 14, 20)
                    ),
                    method = "nonEmpty",
                    parameters = Seq.empty,
                    generics = Seq.empty,
                    returnType = TypeReference("Boolean"),
                    location = Location(validBlogSrcTypes, 14, 11, 14, 31)
                  ),
                  onTrue = MethodCall(
                    expression = AttributeCall(
                      expression = Reference("blog", TypeReference("blog.Blog"), Location(validBlogSrcTypes, 15, 9, 15, 13)),
                      attribute = "tags",
                      returnType = TypeReference("List", Seq(TypeReference("String"))),
                      location = Location(validBlogSrcTypes, 15, 9, 15, 18)
                    ),
                    method = "forAll",
                    parameters = Seq(LambdaExpression(
                      parameterList = Seq(ParameterDefinition("tag", TypeReference("String"), Location(validBlogSrcTypes, 15, 27, 15, 38))),
                      expression = MethodCall(
                        expression = Reference("tag", TypeReference("String"), Location(validBlogSrcTypes, 16, 11, 16, 14)),
                        method = "nonEmpty",
                        parameters = Seq.empty,
                        generics = Seq.empty,
                        returnType = TypeReference("Boolean"),
                        location = Location(validBlogSrcTypes, 16, 11, 16, 25)
                      ),
                      returnType = TypeReference("Boolean"),
                      location = Location(validBlogSrcTypes, 15, 26, 17, 10)
                    )),
                    generics = Seq.empty,
                    returnType = TypeReference("Boolean"),
                    location = Location(validBlogSrcTypes, 15, 9, 17, 11)
                  ),
                  onFalse = Some(BooleanValue(value = true, TypeReference("Boolean"), Location(validBlogSrcTypes, 19, 9, 19, 13))),
                  returnType = TypeReference("Boolean"),
                  location = Location(validBlogSrcTypes, 14, 7, 20, 8)
                ),
                genericTypes = Seq.empty,
                location = Location(validBlogSrcTypes, 13, 5, 21, 6)
              ),
              location = Location(validBlogSrcTypes, 11, 3, 22, 4)
            )
          ),
          inherited = Seq.empty,
          comment = None,
          location = Location(validBlogSrcTypes, 6, 1, 23, 2)
        ),
        DefinedType(
          name = "Comment",
          fullName = "blog.Comment",
          genericTypes = Seq.empty,
          parameters = Seq.empty,
          attributes = Seq(
            AttributeDefinition(
              name = "user",
              typeDeclaration = TypeDeclaration("String", Seq.empty, Seq.empty, Location(validBlogSrcTypes, 26, 9, 26, 15)),
              comment = None,
              verifications = Seq(
                VerificationReference("blog.NonBlank", Seq.empty, Location(validBlogSrcTypes, 26, 16, 26, 34)),
                VerificationReference("blog.ShortString", Seq.empty, Location(validBlogSrcTypes, 26, 35, 26, 56))
              ),
              typeName = None,
              location = Location(validBlogSrcTypes, 26, 3, 26, 56)
            ),
            AttributeDefinition(
              name = "title",
              typeDeclaration = TypeDeclaration(
                typeName = "Option",
                genericTypes = Seq(TypeDeclaration("String", Seq.empty, Seq.empty, Location(validBlogSrcTypes, 27, 17, 27, 23))),
                parameters = Seq.empty,
                location = Location(validBlogSrcTypes, 27, 10, 27, 24)
              ),
              comment = None,
              verifications = Seq.empty,
              typeName = None,
              location = Location(validBlogSrcTypes, 27, 3, 27, 24)
            ),
            AttributeDefinition(
              name = "content",
              typeDeclaration = TypeDeclaration("String", Seq.empty, Seq.empty, Location(validBlogSrcTypes, 28, 12, 28, 18)),
              comment = None,
              verifications = Seq(VerificationReference("blog.NonBlank", Seq.empty, Location(validBlogSrcTypes, 28, 19, 28, 37))),
              typeName = None,
              location = Location(validBlogSrcTypes, 28, 3, 28, 37)
            )
          ),
          verifications = Seq.empty,
          inherited = Seq.empty,
          comment = None,
          location = Location(validBlogSrcTypes, 25, 1, 29, 2)
        ),
        Verification(
          name = "NonBlank",
          fullName = "blog.NonBlank",
          parameters = Seq.empty,
          message = LiteralMessage("The string should not be blank", Location(validBlogSrcVerifications, 4, 3, 4, 35)),
          function = DefinedFunction(
            parameters = Seq(ParameterDefinition("string", TypeReference("String"), Location(validBlogSrcVerifications, 5, 4, 5, 18))),
            body = MethodCall(
              expression = MethodCall(
                expression = Reference("string", TypeReference("String"), Location(validBlogSrcVerifications, 6, 5, 6, 11)),
                method = "trim",
                parameters = Seq.empty,
                generics = Seq.empty,
                returnType = TypeReference("String"),
                location = Location(validBlogSrcVerifications, 6, 5, 6, 18)
              ),
              method = "nonEmpty",
              parameters = Seq.empty,
              generics = Seq.empty,
              returnType = TypeReference("Boolean"),
              location = Location(validBlogSrcVerifications, 6, 5, 6, 29)
            ),
            genericTypes = Seq.empty,
            location = Location(validBlogSrcVerifications, 5, 3, 7, 4)
          ),
          comment = None,
          location = Location(validBlogSrcVerifications, 3, 1, 8, 2)
        ),
        Verification(
          name = "ShortString",
          fullName = "blog.ShortString",
          parameters = Seq.empty,
          message = LiteralMessage("The string should not have more than 25 characters", Location(validBlogSrcVerifications, 11, 3, 11, 55)),
          function = DefinedFunction(
            parameters = Seq(ParameterDefinition("string", TypeReference("String"), Location(validBlogSrcVerifications, 12, 4, 12, 18))),
            body = LogicalExpression(
              operator = LogicalOperator.LowerOrEqual,
              left = AttributeCall(
                expression = Reference("string", TypeReference("String"), Location(validBlogSrcVerifications, 13, 5, 13, 11)),
                attribute = "length",
                returnType = TypeReference("Integer"),
                location = Location(validBlogSrcVerifications, 13, 5, 13, 18)
              ),
              right = IntegerValue(25, TypeReference("Integer"), Location(validBlogSrcVerifications, 13, 22, 13, 24)),
              returnType = TypeReference("Boolean"),
              location = Location(validBlogSrcVerifications, 13, 5, 13, 24)
            ),
            genericTypes = Seq.empty,
            location = Location(validBlogSrcVerifications, 12, 3, 14, 4)
          ),
          comment = None,
          location = Location(validBlogSrcVerifications, 10, 1, 15, 2)
        )
      )
    )
  ))
}