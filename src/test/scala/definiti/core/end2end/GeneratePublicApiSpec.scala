package definiti.core.end2end

import java.nio.file.Paths

import definiti.core.ast.Range
import definiti.core.ast.pure.{AttributeDefinition, ParameterDefinition, TypeReference, VerificationReference}
import definiti.core.ast.structure.{DefinedType, Package, Root, Verification}
import definiti.core.ast.typed._
import definiti.core.{ConfigurationMock, Project, ValidValue}
import org.scalatest.{FlatSpec, Matchers}

class GeneratePublicApiSpec extends FlatSpec with Matchers {
  import GeneratePublicApiSpec._

  "Project.generatePublicAST" should "generate the public API when the project is valid (sample: blog)" in {
    val project = new Project(configuration)
    val expected = ValidValue(blogExpected)
    val output = project.generatePublicAST()
    output should ===(expected)
  }
}

object GeneratePublicApiSpec {
  val configuration = ConfigurationMock(
    source = Paths.get("src/test/resources/samples/blog")
  )

  val blogExpected = Root(Seq(
    Package(
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
              verifications = Seq(VerificationReference("blog.NonBlank", Some("Please provide a title"), Range(7, 16, 7, 59))),
              range = Range(7, 2, 7, 59)
            ),
            AttributeDefinition(
              name = "content",
              typeReference = TypeReference("String"),
              comment = None,
              verifications = Seq(VerificationReference("blog.NonBlank", None, Range(8, 18, 8, 28))),
              range = Range(8, 2, 8, 28)
            ),
            AttributeDefinition(
              name = "tags",
              typeReference = TypeReference("List", Seq(TypeReference("String"))),
              comment = None,
              verifications = Seq.empty,
              range = Range(9, 2, 9, 19)
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
                      expression = Reference("blog", TypeReference("blog.Blog"), Range(14, 10, 14, 10)),
                      attribute = "tags",
                      returnType = TypeReference("List", Seq(TypeReference("String"))),
                      range = Range(9, 2, 9, 19)
                    ),
                    method = "nonEmpty",
                    parameters = Seq.empty,
                    generics = Seq.empty,
                    returnType = TypeReference("Boolean"),
                    range = Range(14, 10, 14, 29)
                  ),
                  onTrue = MethodCall(
                    expression = AttributeCall(
                      expression = Reference("blog", TypeReference("blog.Blog"), Range(15, 8, 15, 8)),
                      attribute = "tags",
                      returnType = TypeReference("List", Seq(TypeReference("String"))),
                      range = Range(9, 2, 9, 19)
                    ),
                    method = "forAll",
                    parameters = Seq(LambdaExpression(
                      parameterList = Seq(ParameterDefinition("tag", TypeReference("String"), Range(15, 26, 15, 31))),
                      expression = MethodCall(
                        expression = Reference("tag", TypeReference("String"), Range(16, 10, 16, 10)),
                        method = "nonEmpty",
                        parameters = Seq.empty,
                        generics = Seq.empty,
                        returnType = TypeReference("Boolean"),
                        range = Range(16, 10, 16, 23)
                      ),
                      returnType = TypeReference("Boolean"),
                      range = Range(15, 25, 17, 8)
                    )),
                    generics = Seq.empty,
                    returnType = TypeReference("Boolean"),
                    range = Range(15, 8, 17, 9)
                  ),
                  onFalse = Some(BooleanValue(value = true, TypeReference("Boolean"), Range(19, 8, 19, 8))),
                  returnType = TypeReference("Boolean"),
                  range = Range(14, 10, 14, 29)
                ),
                genericTypes = Seq.empty,
                range = Range(13, 4, 21, 4)
              ),
              range = Range(11, 2, 22, 2)
            )
          ),
          inherited = Seq.empty,
          comment = None,
          range = Range(6, 0, 23, 0)
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
                VerificationReference("blog.NonBlank", None, Range(26, 15, 26, 25)),
                VerificationReference("blog.ShortString", None, Range(26, 34, 26, 44))
              ),
              range = Range(26, 2, 26, 44)
            ),
            AttributeDefinition(
              name = "title",
              typeReference = TypeReference("Option", Seq(TypeReference("String"))),
              comment = None,
              verifications = Seq.empty,
              range = Range(27, 2, 27, 22)
            ),
            AttributeDefinition(
              name = "content",
              typeReference = TypeReference("String"),
              comment = None,
              verifications = Seq(VerificationReference("blog.NonBlank", None, Range(28, 18, 28, 28))),
              range = Range(28, 2, 28, 28)
            )
          ),
          verifications = Seq.empty,
          inherited = Seq.empty,
          comment = None,
          range = Range(25, 0, 29, 0)
        ),
        Verification(
          name = "NonBlank",
          message = "The string should not be blank",
          function = DefinedFunction(
            parameters = Seq(ParameterDefinition("string", TypeReference("String"), Range(5, 3, 5, 11))),
            body = MethodCall(
              expression = MethodCall(
                expression = Reference("string", TypeReference("String"), Range(6, 4, 6, 4)),
                method = "trim",
                parameters = Seq.empty,
                generics = Seq.empty,
                returnType = TypeReference("String"),
                range = Range(6, 4, 6, 16)
              ),
              method = "nonEmpty",
              parameters = Seq.empty,
              generics = Seq.empty,
              returnType = TypeReference("Boolean"),
              range = Range(6, 4, 6, 27)
            ),
            genericTypes = Seq.empty,
            range = Range(5, 2, 7, 2)
          ),
          comment = None,
          range = Range(3, 0, 8, 0)
        ),
        Verification(
          name = "ShortString",
          message = "The string should not have more than 25 characters",
          function = DefinedFunction(
            parameters = Seq(ParameterDefinition("string", TypeReference("String"), Range(12, 3, 12, 11))),
            body = LowerOrEqual(
              left = AttributeCall(
                expression = Reference("string", TypeReference("String"), Range(13, 4, 13, 4)),
                attribute = "length",
                returnType = TypeReference("Number"),
                range = Range(34, 2, 37, 10)
              ),
              right = NumberValue(25, TypeReference("Number"), Range(13, 21, 13, 21)),
              returnType = TypeReference("Boolean"),
              range = Range(13, 4, 13, 21)
            ),
            genericTypes = Seq.empty,
            range = Range(12, 2, 14, 2)
          ),
          comment = None,
          range = Range(10, 0, 15, 0)
        )
      )
    )
  ))
}