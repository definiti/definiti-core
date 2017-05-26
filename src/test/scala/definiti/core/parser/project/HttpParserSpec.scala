package definiti.core.parser.project

import definiti.core._
import definiti.core.mock.antlr._
import definiti.core.parser.TestConstants._
import definiti.core.utils.ParserUtils.getRangeFromContext
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class HttpParserSpec extends FlatSpec with Matchers with PropertyChecks {
  "HttpParser.processRequirement" should "transform a valid HttpRequirementContext to a valid Requirement" in {
    val input = HttpRequirementContextMock(
      nameToken = TokenMock("MyRequirement"),
      parameterListDefinitionContext = ParameterListDefinitionContextMock(Seq(
        ParameterDefinitionContextMock(
          parameterNameToken = TokenMock("boolean"),
          parameterTypeToken = TokenMock("Boolean"),
          genericTypeListContext = GenericTypeListContextMock(Seq.empty)
        )
      )),
      typeReferenceContext = TypeReferenceContextMock(
        typeNameToken = TokenMock("Boolean"),
        genericTypeListContext = GenericTypeListContextMock(Seq.empty)
      ),
      docComment = TerminalNodeMock("Some doc comment")
    )
    val expected = Requirement(
      name = "MyRequirement",
      packageName = NOT_DEFINED,
      parameters = Seq(
        ParameterDefinition(
          name = "boolean",
          typeReference = TypeReference("Boolean", Seq.empty),
          range = defaultRange
        )
      ),
      returnType = TypeReference("Boolean", Seq.empty),
      comment = Some("Some doc comment"),
      range = defaultRange
    )
    val output = HttpParser.processRequirement(input)
    output should equal(expected)
  }

  "HttpParser.processRequest" should "transform a valid HttpRequestContext to a valid Request" in {
    val input = HttpRequestContextMock(
      nameToken = TokenMock("MyRequest"),
      httpRequestInputContext = HttpRequestInputContextMock(
        httpVerbContext = HttpVerbContextMock(HttpVerbs.POST),
        httpRequestURIContext = HttpRequestURIContextMock(
          Seq(
            HttpRequestURIPartContextMock(Left(TerminalNodeMock("fixed"))),
            HttpRequestURIPartContextMock(Right(
              ParameterDefinitionContextMock(
                parameterNameToken = TokenMock("id"),
                parameterTypeToken = TokenMock("Int"),
                genericTypeListContext = GenericTypeListContextMock(Seq.empty)
              )
            ))
          ),
          ParameterListDefinitionContextMock(Seq(
            ParameterDefinitionContextMock(
              parameterNameToken = TokenMock("myParameter"),
              parameterTypeToken = TokenMock("MyType"),
              genericTypeListContext = GenericTypeListContextMock(Seq.empty)
            )
          ))
        ),
        typeReferenceContext = TypeReferenceContextMock(
          typeNameToken = TokenMock("MyType"),
          genericTypeListContext = GenericTypeListContextMock(Seq.empty)
        )
      ),
      httpRequestRequiringContext = HttpRequestRequiringContextMock(Seq(
        HttpRequestRequirementContextMock(
          httpRequestRequirementReferenceContext = HttpRequestRequirementReferenceContextMock(
            nameToken = TokenMock("MyRequirement"),
            httpParameterListContext = HttpParameterListContextMock(Seq(
              HttpParameterContextMock(nameToken = TokenMock("id"))
            ))
          ),
          httpResultContext = HttpResultContextMock(
            status = Left(HttpStatusNumberContextMock(403)),
            result = Some(Left(TokenMock("Forbidden")))
          )
        )
      )),
      httpRequestReturningContext = HttpRequestReturningContextMock(Seq(
        HttpResultContextMock(
          status = Left(HttpStatusNumberContextMock(200)),
          result = Some(Left(TokenMock("OK")))
        ),
        HttpResultContextMock(
          status = Right(HttpStatusContextMock("BadRequest")),
          result = Some(Right(TypeReferenceContextMock(
            typeNameToken = TokenMock("Boolean"),
            genericTypeListContext = GenericTypeListContextMock(Seq.empty)
          )))
        ),
        HttpResultContextMock(
          status = Left(HttpStatusNumberContextMock(404)),
          result = None
        )
      )),
      docComment = TerminalNodeMock("Some doc here")
    )
    val expected = Request(
      name = "MyRequest",
      input = RequestInput(
        method = "POST",
        requestUri = RequestUri(
          parts = Seq(
            FixedPart("fixed", defaultRange),
            VariablePart(ParameterDefinition(
              "id",
              TypeReference("Int", Seq.empty),
              defaultRange
            ), defaultRange)
          ),
          query = Seq(
            ParameterDefinition(
              name = "myParameter",
              typeReference = TypeReference("MyType", Seq.empty),
              range = defaultRange
            )
          ),
          range = defaultRange
        ),
        inputType = Some(TypeReference(
          typeName = "MyType",
          genericTypes = Seq.empty
        )),
        range = defaultRange
      ),
      requiring = Seq(RequestRequirement(
        requirementReference = RequirementReference(
          name = "MyRequirement",
          parameters = Seq("id"),
          range = defaultRange
        ),
        returning = RequestResult(
          status = 403,
          output = Some(RawOutput("Forbidden", defaultRange)),
          range = defaultRange
        ),
        range = defaultRange
      )),
      returning = Seq(
        RequestResult(
          status = 200,
          output = Some(RawOutput("OK", defaultRange)),
          range = defaultRange
        ),
        RequestResult(
          status = 400,
          output = Some(ReferenceOutput(TypeReference("Boolean", Seq.empty), defaultRange)),
          range = defaultRange
        ),
        RequestResult(
          status = 404,
          output = None,
          range = defaultRange
        )
      ),
      comment = Some("Some doc here"),
      range = defaultRange
    )
    val output = HttpParser.processRequest(input)
    output should equal(expected)
  }
}
