package definiti.core.validation

import definiti.core._
import definiti.core.generators.HttpASTGenerator
import definiti.core.parser.TestConstants._
import definiti.core.parser.project.CoreParser
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class HttpValidationSpec extends FlatSpec with Matchers with PropertyChecks with CoreParser {
  private val httpValidation = new HttpValidation()

  "HttpValidation.validateRequirement" should "accept a valid requirement" in {
    implicit val context = coreContext
    forAll(HttpASTGenerator.anyRequirement) { requirement =>
      val input = requirement.copy(
        parameters = Seq(ParameterDefinition("valid", TypeReference("Boolean", Seq.empty), defaultRange)),
        returnType = TypeReference("Unit", Seq.empty)
      )
      val expected = Valid
      val output = httpValidation.validateRequirement(input)
      output should equal(expected)
    }
  }

  it should "not accept a requirement with unknown parameter type or unknown return type" in {
    implicit val context = coreContext
    forAll(HttpASTGenerator.anyRequirement) { requirement =>
      val input = requirement.copy(
        parameters = Seq(ParameterDefinition("valid", TypeReference("Unknown", Seq.empty), defaultRange)),
        returnType = TypeReference("NotRegistered", Seq.empty),
        range = defaultRange
      )
      val expected = Invalid(Seq(
        ASTError("Undefined type: Unknown", defaultRange),
        ASTError("Undefined type: NotRegistered", defaultRange)
      ))
      val output = httpValidation.validateRequirement(input)
      output should equal(expected)
    }
  }

  "HttpValidation.validateRequestInput" should "accept a valid RequestInput" in {
    implicit val context = coreContext
    forAll(HttpASTGenerator.anyRequestInput) { requestInput =>
      val input = requestInput.copy(
        requestUri = RequestUri(
          parts = Seq(
            FixedPart("fixed", defaultRange),
            VariablePart(ParameterDefinition("var", TypeReference("Boolean", Seq.empty), defaultRange), defaultRange)
          ),
          query = Seq(ParameterDefinition("query", TypeReference("Number", Seq.empty), defaultRange)),
          range = defaultRange
        ),
        inputType = Some(TypeReference("String", Seq.empty))
      )
      val expected = Valid
      val output = httpValidation.validateRequestInput(input)
      output should equal(expected)
    }
  }

  it should "not accept a RequestInput with invalid types" in {
    implicit val context = coreContext
    forAll(HttpASTGenerator.anyRequestInput) { requestInput =>
      val input = requestInput.copy(
        requestUri = RequestUri(
          parts = Seq(
            FixedPart("fixed", defaultRange),
            VariablePart(ParameterDefinition("var", TypeReference("Unknown", Seq.empty), defaultRange), defaultRange)
          ),
          query = Seq(ParameterDefinition("query", TypeReference("Invalid", Seq.empty), defaultRange)),
          range = defaultRange
        ),
        inputType = Some(TypeReference("TryAgain", Seq.empty)),
        range = defaultRange
      )
      val expected = Invalid(Seq(
        ASTError("Undefined type: Unknown", defaultRange),
        ASTError("Undefined type: Invalid", defaultRange),
        ASTError("Undefined type: TryAgain", defaultRange)
      ))
      val output = httpValidation.validateRequestInput(input)
      output should equal(expected)
    }
  }

  "HttpValidation.validateRequestRequirement" should "accept a valid RequestRequirement" in {
    implicit val context = contextWithRequirement
    val cases = for {
      requestRequirement <- HttpASTGenerator.anyRequestRequirement
      requestInput <- HttpASTGenerator.anyRequestInput
    } yield (requestRequirement, requestInput)
    forAll(cases) { case (requestRequirement, requestInput) =>
      val requestRequirementInput = requestRequirement.copy(
        requirementReference = RequirementReference(
          name = "IsAcceptable",
          parameters = Seq("id"),
          range = defaultRange
        ),
        returning = RequestResult(
          status = 400,
          output = Some(ReferenceOutput(TypeReference("Boolean", Seq.empty), defaultRange)),
          range = defaultRange
        )
      )
      val requestInputInput = requestInput.copy(
        requestUri = RequestUri(
          parts = Seq(
            VariablePart(ParameterDefinition("id", TypeReference("Number", Seq.empty), defaultRange), defaultRange)
          ),
          query = Seq(),
          range = defaultRange
        ),
        inputType = None
      )
      val expected = Valid
      val result = httpValidation.validateRequestRequirement(requestRequirementInput, requestInputInput)
      result should equal(expected)
    }
  }

  it should "not accept a RequestRequirement with invalid types or reference" in {
    implicit val context = contextWithRequirement
    val cases = for {
      requestRequirement <- HttpASTGenerator.anyRequestRequirement
      requestInput <- HttpASTGenerator.anyRequestInput
    } yield (requestRequirement, requestInput)
    forAll(cases) { case (requestRequirement, requestInput) =>
      val requestRequirementInput = requestRequirement.copy(
        requirementReference = RequirementReference(
          name = "IsAcceptable",
          parameters = Seq("unknown"),
          range = defaultRange
        ),
        returning = RequestResult(
          status = 400,
          output = Some(ReferenceOutput(TypeReference("Invalid", Seq.empty), defaultRange)),
          range = defaultRange
        )
      )
      val requestInputInput = requestInput.copy(
        requestUri = RequestUri(
          parts = Seq(
            VariablePart(ParameterDefinition("id", TypeReference("Number", Seq.empty), defaultRange), defaultRange)
          ),
          query = Seq(),
          range = defaultRange
        ),
        inputType = None
      )
      val expected = Invalid(Seq(
        ASTError("Undefined variable: unknown", defaultRange),
        ASTError("Undefined type: Invalid", defaultRange)
      ))
      val result = httpValidation.validateRequestRequirement(requestRequirementInput, requestInputInput)
      result should equal(expected)
    }
  }

  "HttpValidation.validateRequestResult" should "accept a valid RequestResult" in {
    implicit val context = coreContext
    forAll(HttpASTGenerator.anyRequestResult) { requestResult =>
      val input = requestResult.copy(
        output = Some(ReferenceOutput(TypeReference("Boolean", Seq.empty), defaultRange))
      )
      val expected = Valid
      val result = httpValidation.validateRequestResult(input)
      result should equal(expected)
    }
  }

  it should "not accept a RequestRequirement with invalid types or reference" in {
    implicit val context = coreContext
    forAll(HttpASTGenerator.anyRequestResult) { requestResult =>
      val input = requestResult.copy(
        output = Some(ReferenceOutput(TypeReference("Invalid", Seq.empty), defaultRange))
      )
      val expected = Invalid(Seq(
        ASTError("Undefined type: Invalid", defaultRange)
      ))
      val result = httpValidation.validateRequestResult(input)
      result should equal(expected)
    }
  }

  private lazy val contextWithRequirement = coreContext.copy(
    requirements = coreContext.requirements ++ Seq(
      Requirement(
        name = "IsAcceptable",
        packageName = "some",
        parameters = Seq(ParameterDefinition("myVar", TypeReference("Number", Seq.empty), defaultRange)),
        returnType = TypeReference("String", Seq.empty),
        comment = None,
        range = defaultRange
      )
    )
  )
}
