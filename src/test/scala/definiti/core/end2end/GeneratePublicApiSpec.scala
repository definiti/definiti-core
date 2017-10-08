package definiti.core.end2end

import definiti.core._
import definiti.core.ast._

class GeneratePublicApiSpec extends EndToEndSpec {
  import GeneratePublicApiSpec._
  import ValidationMatchers._

  "Project.generatePublicAST" should "generate the public API when the project is valid (sample: blog)" in {
    val expected = ValidValue(validBlogExpected)
    val output = processDirectory("blog")
    output should beValidated(expected)
  }

  it should "return an error when the project is invalid (sample: invalid.blog)" in {
    val expected = Invalid(invalidBlogExpected)
    val output = processDirectory("invalid.blog")
    output should beValidated[Root](expected)
  }

  it should "return an error when the project is invalid (sample: invalid.blog2)" in {
    val expected = Invalid(invalidBlog2Expected)
    val output = processDirectory("invalid.blog2")
    output should beValidated[Root](expected)
  }
}

object GeneratePublicApiSpec {
  val validBlogExpected = Root(Seq(
    Namespace(
      name = "blog",
      elements = Seq(
        DefinedType(
          name = "Blog",
          genericTypes = Seq.empty,
          attributes = Seq(
            AttributeDefinition(
              name = "title",
              typeReference = TypeReference("String"),
              comment = None,
              verifications = Seq(VerificationReference("blog.NonBlank", Some("Please provide a title"), Range(7, 17, 7, 61))),
              range = Range(7, 3, 7, 61)
            ),
            AttributeDefinition(
              name = "content",
              typeReference = TypeReference("String"),
              comment = None,
              verifications = Seq(VerificationReference("blog.NonBlank", None, Range(8, 19, 8, 37))),
              range = Range(8, 3, 8, 37)
            ),
            AttributeDefinition(
              name = "tags",
              typeReference = TypeReference("List", Seq(TypeReference("String"))),
              comment = None,
              verifications = Seq.empty,
              range = Range(9, 3, 9, 21)
            )
          ),
          verifications = Seq(
            TypeVerification(
              message = "No tag can be empty",
              function = DefinedFunction(
                parameters = Seq(ParameterDefinition("blog", TypeReference("blog.Blog"), Range(13, 246, 13, 249))),
                body = Condition(
                  condition = MethodCall(
                    expression = AttributeCall(
                      expression = Reference("blog", TypeReference("blog.Blog"), Range(14, 11, 14, 15)),
                      attribute = "tags",
                      returnType = TypeReference("List", Seq(TypeReference("String"))),
                      range = Range(9, 3, 9, 21)
                    ),
                    method = "nonEmpty",
                    parameters = Seq.empty,
                    generics = Seq.empty,
                    returnType = TypeReference("Boolean"),
                    range = Range(14, 11, 14, 31)
                  ),
                  onTrue = MethodCall(
                    expression = AttributeCall(
                      expression = Reference("blog", TypeReference("blog.Blog"), Range(15, 9, 15, 13)),
                      attribute = "tags",
                      returnType = TypeReference("List", Seq(TypeReference("String"))),
                      range = Range(9, 3, 9, 21)
                    ),
                    method = "forAll",
                    parameters = Seq(LambdaExpression(
                      parameterList = Seq(ParameterDefinition("tag", TypeReference("String"), Range(15, 27, 15, 38))),
                      expression = MethodCall(
                        expression = Reference("tag", TypeReference("String"), Range(16, 11, 16, 14)),
                        method = "nonEmpty",
                        parameters = Seq.empty,
                        generics = Seq.empty,
                        returnType = TypeReference("Boolean"),
                        range = Range(16, 11, 16, 25)
                      ),
                      returnType = TypeReference("Boolean"),
                      range = Range(15, 26, 17, 10)
                    )),
                    generics = Seq.empty,
                    returnType = TypeReference("Boolean"),
                    range = Range(15, 9, 17, 11)
                  ),
                  onFalse = Some(BooleanValue(value = true, TypeReference("Boolean"), Range(19, 9, 19, 13))),
                  returnType = TypeReference("Boolean"),
                  range = Range(14, 7, 20, 8)
                ),
                genericTypes = Seq.empty,
                range = Range(13, 5, 21, 6)
              ),
              range = Range(11, 3, 22, 4)
            )
          ),
          inherited = Seq.empty,
          comment = None,
          range = Range(6, 1, 23, 2)
        ),
        DefinedType(
          name = "Comment",
          genericTypes = Seq.empty,
          attributes = Seq(
            AttributeDefinition(
              name = "user",
              typeReference = TypeReference("String"),
              comment = None,
              verifications = Seq(
                VerificationReference("blog.NonBlank", None, Range(26, 16, 26, 34)),
                VerificationReference("blog.ShortString", None, Range(26, 35, 26, 56))
              ),
              range = Range(26, 3, 26, 56)
            ),
            AttributeDefinition(
              name = "title",
              typeReference = TypeReference("Option", Seq(TypeReference("String"))),
              comment = None,
              verifications = Seq.empty,
              range = Range(27, 3, 27, 24)
            ),
            AttributeDefinition(
              name = "content",
              typeReference = TypeReference("String"),
              comment = None,
              verifications = Seq(VerificationReference("blog.NonBlank", None, Range(28, 19, 28, 37))),
              range = Range(28, 3, 28, 37)
            )
          ),
          verifications = Seq.empty,
          inherited = Seq.empty,
          comment = None,
          range = Range(25, 1, 29, 2)
        ),
        Verification(
          name = "NonBlank",
          message = "The string should not be blank",
          function = DefinedFunction(
            parameters = Seq(ParameterDefinition("string", TypeReference("String"), Range(5, 4, 5, 18))),
            body = MethodCall(
              expression = MethodCall(
                expression = Reference("string", TypeReference("String"), Range(6, 5, 6, 11)),
                method = "trim",
                parameters = Seq.empty,
                generics = Seq.empty,
                returnType = TypeReference("String"),
                range = Range(6, 5, 6, 18)
              ),
              method = "nonEmpty",
              parameters = Seq.empty,
              generics = Seq.empty,
              returnType = TypeReference("Boolean"),
              range = Range(6, 5, 6, 29)
            ),
            genericTypes = Seq.empty,
            range = Range(5, 3, 7, 4)
          ),
          comment = None,
          range = Range(3, 1, 8, 2)
        ),
        Verification(
          name = "ShortString",
          message = "The string should not have more than 25 characters",
          function = DefinedFunction(
            parameters = Seq(ParameterDefinition("string", TypeReference("String"), Range(12, 4, 12, 18))),
            body = LogicalExpression(
              operator = LogicalOperator.LowerOrEqual,
              left = AttributeCall(
                expression = Reference("string", TypeReference("String"), Range(13, 5, 13, 11)),
                attribute = "length",
                returnType = TypeReference("Number"),
                range = Range(34, 3, 37, 17)
              ),
              right = NumberValue(25, TypeReference("Number"), Range(13, 22, 13, 24)),
              returnType = TypeReference("Boolean"),
              range = Range(13, 5, 13, 24)
            ),
            genericTypes = Seq.empty,
            range = Range(12, 3, 14, 4)
          ),
          comment = None,
          range = Range(10, 1, 15, 2)
        )
      )
    )
  ))

  val invalidBlogExpected = Seq(
    ASTError("Expected boolean expression, got: class unit", Range(13, 5, 15, 6)),
    ASTError("Undefined verification: Unexisting", Range(8, 3, 8, 39)),
    ASTError("Expected boolean expression, got: class unit", Range(14, 7, 20, 8)),
    ASTError("Undefined type: Something", Range(28, 3, 28, 40))
  )

  val invalidBlog2Expected = Seq(
    ASTError("Unknown method String.noEmpty", Range(6, 5, 6, 28))
  )
}