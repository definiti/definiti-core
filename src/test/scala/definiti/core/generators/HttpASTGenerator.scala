package definiti.core.generators

import definiti.core._
import org.scalacheck.Gen

object HttpASTGenerator {
  lazy val anyHttpAST: Gen[HttpAST] = for {
    requirements <- Gen.listOf(anyRequirement)
    requests <- Gen.listOf(anyRequest)
    range <- ASTGenerator.anyRange
  } yield {
    HttpAST(
      requirements = requirements,
      requests = requests,
      range = range
    )
  }

  lazy val anyRequirement: Gen[Requirement] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    parameters <- Gen.listOf(ASTGenerator.anyParameterDefinition)
    returnType <- ASTGenerator.anyTypeReference
    comment <- Gen.option(ASTGenerator.anyString)
    range <- ASTGenerator.anyRange
  } yield {
    Requirement(
      name = name,
      packageName = packageName,
      parameters = parameters,
      returnType = returnType,
      comment = comment,
      range = range
    )
  }

  lazy val anyRequest: Gen[Request] = for {
    name <- ASTGenerator.anyIdentifier
    packageName <- ASTGenerator.anyPackageName
    input <- anyRequestInput
    requiring <- Gen.listOf(anyRequestRequirement)
    returning <- Gen.listOf(anyRequestResult)
    comment <- Gen.option(ASTGenerator.anyString)
    range <- ASTGenerator.anyRange
  } yield {
    Request(
      name = name,
      packageName = packageName,
      input = input,
      requiring = requiring,
      returning = returning,
      comment = comment,
      range = range
    )
  }

  lazy val anyRequestInput: Gen[RequestInput] = for {
    method <- anyMethod
    requestUri <- anyRequestUri
    inputType <- Gen.option(ASTGenerator.anyTypeReference)
    range <- ASTGenerator.anyRange
  } yield {
    RequestInput(
      method = method,
      requestUri = requestUri,
      inputType = inputType,
      range = range
    )
  }

  lazy val anyMethod: Gen[String] = Gen.oneOf(
    "CONNECT",
    "DELETE",
    "GET",
    "HEAD",
    "PATCH",
    "POST",
    "PUT",
    "OPTIONS",
    "TRACE"
  )

  lazy val anyRequestUri: Gen[RequestUri] = for {
    parts <- Gen.listOf(anyRequestUriPart)
    query <- Gen.listOf(ASTGenerator.anyParameterDefinition)
    range <- ASTGenerator.anyRange
  } yield {
    RequestUri(
      parts = parts,
      query = query,
      range = range
    )
  }

  lazy val anyRequestUriPart: Gen[RequestUriPart] = Gen.oneOf(anyFixedPart, anyVariablePart)

  lazy val anyFixedPart: Gen[FixedPart] = for {
    text <- ASTGenerator.anyString
    range <- ASTGenerator.anyRange
  } yield {
    FixedPart(
      text = text,
      range = range
    )
  }

  lazy val anyVariablePart: Gen[VariablePart] = for {
    parameterDefinition <- ASTGenerator.anyParameterDefinition
    range <- ASTGenerator.anyRange
  } yield {
    VariablePart(
      parameterDefinition = parameterDefinition,
      range = range
    )
  }

  lazy val anyRequestRequirement: Gen[RequestRequirement] = for {
    requirementReference <- anyRequirementReference
    returning <- anyRequestResult
    range <- ASTGenerator.anyRange
  } yield {
    RequestRequirement(
      requirementReference = requirementReference,
      returning = returning,
      range = range
    )
  }

  lazy val anyRequirementReference: Gen[RequirementReference] = for {
    name <- ASTGenerator.anyIdentifier
    parameters <- Gen.listOf(ASTGenerator.anyIdentifier)
    range <- ASTGenerator.anyRange
  } yield {
    RequirementReference(
      name = name,
      parameters = parameters,
      range = range
    )
  }

  lazy val anyRequestResult: Gen[RequestResult] = for {
    status <- Gen.posNum[Int]
    output <- Gen.option(anyRequestOutput)
    range <- ASTGenerator.anyRange
  } yield {
    RequestResult(
      status = status,
      output = output,
      range = range
    )
  }

  lazy val anyRequestOutput: Gen[RequestOutput] = Gen.oneOf(anyRawOutput, anyReferenceOutput)

  lazy val anyRawOutput: Gen[RawOutput] = for {
    text <- ASTGenerator.anyString
    range <- ASTGenerator.anyRange
  } yield {
    RawOutput(
      text = text,
      range = range
    )
  }

  lazy val anyReferenceOutput: Gen[ReferenceOutput] = for {
    typeReference <- ASTGenerator.anyTypeReference
    range <- ASTGenerator.anyRange
  } yield {
    ReferenceOutput(
      typeReference = typeReference,
      range = range
    )
  }
}
